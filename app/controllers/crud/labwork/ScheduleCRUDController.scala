package controllers.crud.labwork

import java.util.UUID

import controllers.crud.AbstractCRUDController
import models.{Room, Course, UriGenerator}
import models.labwork._
import org.openrdf.model.Value
import models.users.{User, Employee}
import org.w3.banana.RDFPrefix
import org.w3.banana.binder.{ClassUrisFor, FromPG, ToPG}
import org.w3.banana.sesame.Sesame
import play.api.libs.json.{JsValue, Json, Reads, Writes}
import play.api.mvc.Result
import services._
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import store.{Namespace, SesameRepository}
import utils.{Gen, LwmMimeType}
import models.security.Permissions._
import utils.RequestOps._
import scala.collection.Map
import scala.util.{Failure, Success, Try}
import ScheduleCRUDController._

object ScheduleCRUDController {

  val courseAttribute = "course"
  val supervisorAttribute = "supervisor"

  def competitive(labwork: UUID, repository: SesameRepository): Try[Set[ScheduleG]] = {
    scheduleFor(labwork, repository) map { set =>
      set.flatMap(schedule => toScheduleG(schedule, repository))
    }
  }

  private def scheduleFor(labwork: UUID, repository: SesameRepository): Try[Set[Schedule]] = {
    lazy val lwm = LWMPrefix[repository.Rdf]
    val bindings = Bindings[repository.Rdf](repository.namespace)

    import store.sparql.select._
    import store.sparql.select
    import bindings.ScheduleBinding._
    import bindings.ScheduleEntryBinding._
    import utils.Ops._
    import TraverseInstances._
    import MonadInstances.{tryM, listM}

    lazy val id = Labwork.generateUri(labwork)(repository.namespace)

    val query = select distinct "schedules" where {
      ^(s(id), p(lwm.course), v("courseid")).
        ^(s(id), p(lwm.semester), v("semester")).
        ^(v("course"), p(lwm.id), v("courseid")).
        ^(v("course"), p(lwm.semesterIndex), v("semesterIndex")).
        ^(v("labwork"), p(lwm.semester), v("semester")).
        ^(v("labwork"), p(lwm.course), v("course2id")).
        ^(v("course2"), p(lwm.id), v("course2id")).
        ^(v("course2"), p(lwm.semesterIndex), v("semesterIndex")).
        ^(v("labwork"), p(lwm.id), v("labworkid")).
        ^(v("schedules"), p(lwm.labwork), v("labworkid"))
    }

      repository.prepareQuery(query).
      select(_.get("schedules")).
      transform(_.fold(List.empty[Value])(identity)).
      map(_.stringValue()).
      requestAll(repository.getMany[Schedule]).
      run
  }

  private def toScheduleG(schedule: Schedule, repository: SesameRepository): Option[ScheduleG] = {
    val bindings = Bindings[repository.Rdf](repository.namespace)
    import bindings.GroupBinding.groupBinder
    import bindings.GroupBinding._

    import utils.Ops._
    import MonadInstances._

    val maybeEntries =
      (schedule.entries flatMap { entry =>
        val group = repository.get[Group](Group.generateUri(entry.group)(repository.namespace)).toOption
        group.peek(g => ScheduleEntryG(entry.start, entry.end, entry.date, entry.room, entry.supervisor, g))
      }).sequence


    maybeEntries map (entries => ScheduleG(schedule.labwork, entries.toVector, schedule.id))
  }

  private def toSchedule(scheduleG: ScheduleG): Schedule = {
    val entries = scheduleG.entries.map(e => ScheduleEntry(e.start, e.end, e.date, e.room, e.supervisor, e.group.id)).toSet
    Schedule(scheduleG.labwork, entries, published = false, scheduleG.id)
  }
}

class ScheduleCRUDController(val repository: SesameRepository,
                             val sessionService: SessionHandlingService,
                             val namespace: Namespace,
                             val roleService: RoleService,
                             val scheduleGenesisService: ScheduleGenesisServiceLike,
                             val reportCardService: ReportCardServiceLike
                            ) extends AbstractCRUDController[ScheduleProtocol, Schedule] {

  override implicit def rdfWrites: ToPG[Sesame, Schedule] = defaultBindings.ScheduleBinding.scheduleBinder

  override implicit def rdfReads: FromPG[Sesame, Schedule] = defaultBindings.ScheduleBinding.scheduleBinder

  override implicit def classUrisFor: ClassUrisFor[Sesame, Schedule] = defaultBindings.ScheduleBinding.classUri

  override implicit def uriGenerator: UriGenerator[Schedule] = Schedule

  override protected def fromInput(input: ScheduleProtocol, existing: Option[Schedule]): Schedule = existing match {
    case Some(schedule) => Schedule(input.labwork, input.entries, input.published, schedule.id)
    case None => Schedule(input.labwork, input.entries, input.published, Schedule.randomUUID)
  }

  override implicit def reads: Reads[ScheduleProtocol] = Schedule.reads

  override implicit def writes: Writes[Schedule] = Schedule.writes

  override implicit val mimeType: LwmMimeType = LwmMimeType.scheduleV1Json

  // TODO FILTER WITH ASSIGNMENT PLAN
  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Schedule]): Try[Set[Schedule]] = {
    import defaultBindings.LabworkBinding.labworkBinder
    import utils.Ops.MonadInstances.listM
    import store.sparql.select
    import store.sparql.select._
    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    queryString.foldRight(Try[Set[Schedule]](all)) {
      case ((`courseAttribute`, values), t) =>
        val query = select ("labworks") where {
          ^(v("labworks"), p(rdf.`type`), s(lwm.Labwork)).
            ^(v("labworks"), p(lwm.course), s(Course.generateUri(UUID.fromString(values.head))(namespace)))
        }

        repository.prepareQuery(query).
          select(_.get("labworks")).
          transform(_.fold(List.empty[Value])(identity)).
          map(_.stringValue).
          requestAll(repository.getMany[Labwork](_)).
          requestAll[Set, Schedule](labworks => t.map(_.filter(s => labworks.exists(_.id == s.labwork)))).
          run

      case ((`supervisorAttribute`, values), t) =>
        for {
          schedules <- t; supervisor <- Try(UUID.fromString(values.head))
        } yield
          schedules.filter(_.entries.exists(_.supervisor == supervisor)).map { s =>
            Schedule(s.id, s.entries.filter(_.supervisor == supervisor), s.published, s.id)
          }

      case ((_, _), set) => Failure(new Throwable("Unknown attribute"))
    }
  }

  def createFrom(course: String, labwork: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    create(NonSecureBlock)(rebase(Schedule.generateBase))
  }

  def updateFrom(course: String, labwork: String, schedule: String) = restrictedContext(course)(Update) asyncContentTypedAction { implicit request =>
    update(schedule, NonSecureBlock)(rebase(Schedule.generateBase(UUID.fromString(schedule))))
  }

  def createAtomicFrom(course: String, labwork: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    createAtomic(NonSecureBlock)(rebase(Schedule.generateBase))
  }

  def updateAtomicFrom(course: String, labwork: String, schedule: String) = restrictedContext(course)(Update) asyncContentTypedAction { implicit request =>
    updateAtomic(schedule, NonSecureBlock)(rebase(Schedule.generateBase(UUID.fromString(schedule))))
  }

  def allFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    all(NonSecureBlock)(rebase(Schedule.generateBase, courseAttribute -> Seq(course)))
  }

  def allAtomicFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    allAtomic(NonSecureBlock)(rebase(Schedule.generateBase, courseAttribute -> Seq(course)))
  }

  def getFrom(course: String, labwork: String, schedule: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    get(schedule, NonSecureBlock)(rebase(Schedule.generateBase(UUID.fromString(schedule))))
  }

  def getAtomicFrom(course: String, labwork: String, schedule: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    getAtomic(schedule, NonSecureBlock)(rebase(Schedule.generateBase(UUID.fromString(schedule))))
  }

  def deleteFrom(course: String, labwork: String, schedule: String) = restrictedContext(course)(Delete) asyncAction { implicit request =>
    delete(schedule, NonSecureBlock)(rebase(Schedule.generateBase(UUID.fromString(schedule))))
  }

  def previewFrom(course: String, labwork: String) = preview(course, labwork)( gen =>
    Success(Some(Ok(Json.obj(
      "status" -> "OK",
      "schedule" -> Json.toJson(ScheduleCRUDController.toSchedule(gen.elem)),
      "number of conflicts" -> gen.evaluate.err.size // TODO serialize conflicts
    ))))
  )

  def previewAtomicFrom(course: String, labwork: String) = preview(course, labwork) { gen =>
    import utils.Ops._
    import MonadInstances.{tryM, optM}

    gen.map(ScheduleCRUDController.toSchedule).map(atomize).elem.peek( json =>
      Ok(Json.obj(
        "status" -> "OK",
        "schedule" -> json,
        "number of conflicts" -> gen.evaluate.err.size // TODO serialize conflicts
      ))
    )
  }

  private def preview(course: String, labwork: String)(f: Gen[ScheduleG, Conflict, Int] => Try[Option[Result]]) = restrictedContext(course)(Create) action { implicit request =>
    import ScheduleCRUDController._
    import utils.Ops._
    import MonadInstances.{tryM, optM}
    import TraverseInstances.{travT, travO}

    implicit val gb = defaultBindings.GroupBinding.groupBinder
    implicit val gcu = defaultBindings.GroupBinding.classUri
    implicit val tb = defaultBindings.TimetableBinding.timetableBinder
    implicit val tcu = defaultBindings.TimetableBinding.classUri
    implicit val ab = defaultBindings.AssignmentPlanBinding.assignmentPlanBinder
    implicit val abu = defaultBindings.AssignmentPlanBinding.classUri

    val id = UUID.fromString(labwork)

    val genesis = for {
      groups <- repository.get[Group].map(_.filter(_.labwork == id))
      timetable <- repository.get[Timetable].map(_.find(_.labwork == id))
      plans <- repository.get[AssignmentPlan].map(_.find(_.labwork == id))
      comp <- competitive(id, repository)
    } yield {
      for {
        t <- timetable if t.entries.nonEmpty
        p <- plans if p.entries.nonEmpty
        g <- if (groups.nonEmpty) Some(groups) else None
      } yield scheduleGenesisService.generate(t, g, p, comp.toVector)._1
    }

    genesis.flatPeek(f) match {
      case Success(s) =>
        s match {
          case Some(result) => result
          case None =>
            NotFound(Json.obj(
              "status" -> "KO",
              "message" -> "No such element..."
            ))
        }
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }

  def publish(course: String, labwork: String, schedule: String) = restrictedContext(course)(Create) action { request =>
    import utils.Ops._
    import utils.Ops.MonadInstances.{tryM, optM, setM}
    import utils.Ops.NaturalTrasformations._
    import utils.Ops.TraverseInstances._
    import defaultBindings.AssignmentPlanBinding._
    import store.sparql.select
    import store.sparql.select._
    import defaultBindings.ReportCardBinding._

    val id = UUID.fromString(schedule)
    val uri = Schedule.generateUri(id)(namespace)
    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    val query = select ("plan") where {
      ^(v("plan"), p(rdf.`type`), s(lwm.AssignmentPlan)).
      ^(v("plan"), p(lwm.labwork), s(Labwork.generateUri(UUID.fromString(labwork))(namespace)))
    }

    val result = repository.prepareQuery(query).
      select(_.get("plan")).
      changeTo(_.headOption).
      request(value => repository.get[AssignmentPlan](value.stringValue())).
      request(plan =>  repository.get[Schedule](uri).peek((_, plan))(tryM, optM)).
      request {
        case ((s, plan)) =>
          val published = Schedule(s.labwork, s.entries, published = true, s.id)
          repository.update(published).map(_ => Option((s, plan)))
      }.
      flatMap(t => ScheduleCRUDController.toScheduleG(t._1, repository).map((_, t._2))).
      transform(opt => opt.map(t => Set(t)).getOrElse(Set.empty[(ScheduleG, AssignmentPlan)])).
      flatMap(t => reportCardService.reportCards(t._1, t._2)).
      requestAll(reports => repository.addMany[ReportCard](reports)).
      run

    result match {
      case Success(set) if set.nonEmpty =>
        Ok(Json.obj(
          "status" -> "OK",
          "message" -> "Published"
        ))
      case Success(_) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "message" -> "Error while creating report cards"
        ))
      case Failure(e) =>
        InternalServerError(Json.obj(
          "status" -> "KO",
          "errors" -> e.getMessage
        ))
    }
  }

  override protected def compareModel(input: ScheduleProtocol, output: Schedule): Boolean = input.entries == output.entries && input.published == output.published

  override protected def atomize(output: Schedule): Try[Option[JsValue]] = {
    import defaultBindings.LabworkBinding._
    import defaultBindings.RoomBinding.roomBinder
    import defaultBindings.EmployeeBinding.employeeBinder
    import defaultBindings.GroupBinding.groupBinder
    import Schedule._
    import ScheduleEntry._

    for {
      labwork <- repository.get[Labwork](Labwork.generateUri(output.labwork)(namespace))
      rooms <- repository.getMany[Room](output.entries.map(e => Room.generateUri(e.room)(namespace)))
      supervisors <- repository.getMany[Employee](output.entries.map(e => User.generateUri(e.supervisor)(namespace)))
      groups <- repository.getMany[Group](output.entries.map(e => Group.generateUri(e.group)(namespace)))
    } yield {
      labwork.map { l =>
        val entries = output.entries.foldLeft(Set.empty[ScheduleEntryAtom]) { (newSet, e) =>
          (for {
            r <- rooms.find(_.id == e.room)
            s <- supervisors.find(_.id == e.supervisor)
            g <- groups.find(_.id == e.group)
          } yield ScheduleEntryAtom(e.start, e.end, e.date, r, s, g)) match {
            case Some(atom) => newSet + atom
            case None => newSet
          }
        }
        Json.toJson(ScheduleAtom(l, entries, output.published, output.id))(Schedule.atomicWrites)
      }
    }
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, schedule.create)
    case Get => SecureBlock(restrictionId, schedule.get)
    case GetAll => SecureBlock(restrictionId, schedule.getAll)
    case Update => SecureBlock(restrictionId, schedule.update)
    case Delete => SecureBlock(restrictionId, schedule.delete)
  }
}