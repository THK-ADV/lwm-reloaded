package controllers

import java.util.UUID

import models.Permissions.{schedule, god}
import models._
import org.openrdf.model.Value
import org.w3.banana.RDFPrefix
import org.w3.banana.sesame.Sesame
import play.api.libs.json._
import play.api.mvc.{Action, Request}
import services._
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import store.bind.Descriptor.Descriptor
import store.{Namespace, SesameRepository}
import utils.{Attempt, Gen, LwmMimeType}
import controllers.ScheduleController._
import scala.collection.Map
import scala.util.{Failure, Try}

object ScheduleController {

  def toScheduleG(schedule: Schedule, repository: SesameRepository): Option[ScheduleG] = {
    val bindings = Bindings[repository.Rdf](repository.namespace)
    import bindings.GroupDescriptor
    import utils.Ops._
    import MonadInstances._

    val maybeEntries =
      (schedule.entries flatMap { entry =>
        val group = repository.get[Group](Group.generateUri(entry.group)(repository.namespace)).toOption
        group.peek(g => ScheduleEntryG(entry.start, entry.end, entry.date, entry.room, entry.supervisor, g))
      }).sequence


    maybeEntries map (entries => ScheduleG(schedule.labwork, entries.toVector, schedule.id))
  }

  def toSchedule(scheduleG: ScheduleG): Schedule = {
    val entries = scheduleG.entries.map(e => ScheduleEntry(scheduleG.labwork, e.start, e.end, e.date, e.room, e.supervisor, e.group.id)).toSet
    Schedule(scheduleG.labwork, entries, None, scheduleG.id)
  }

  val labworkAttribute = "labwork"

  val popAttribute = "pops"
  val genAttribute = "gens"
  val eliteAttribute = "elites"
}

// TODO ScheduleProtocol, ScheduleG -> ScheduleAtom, PUT for Schedules
class ScheduleController(val repository: SesameRepository, val sessionService: SessionHandlingService, implicit val namespace: Namespace, val roleService: RoleService, val scheduleGenesisService: ScheduleGenesisServiceLike, val groupService: GroupServiceLike)
  extends AbstractCRUDController[Schedule, Schedule, ScheduleAtom] {

  override implicit val mimeType: LwmMimeType = LwmMimeType.scheduleV1Json

  override implicit val descriptor: Descriptor[Sesame, Schedule] = defaultBindings.ScheduleDescriptor

  override implicit val descriptorAtom: Descriptor[Sesame, ScheduleAtom] = defaultBindings.ScheduleAtomDescriptor

  override implicit val reads: Reads[Schedule] = Schedule.reads

  override implicit val writes: Writes[Schedule] = Schedule.writes

  override implicit val writesAtom: Writes[ScheduleAtom] = Schedule.writesAtom

  override implicit val uriGenerator: UriGenerator[Schedule] = Schedule

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, schedule.create)
    case Delete => SecureBlock(restrictionId, schedule.delete)
    case Get => SecureBlock(restrictionId, schedule.get)
    case _ => PartialSecureBlock(god)
  }

  def allFrom(course: String, labwork: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    filteredOnly(NonSecureBlock)(rebase(labworkAttribute -> Seq(labwork)))
  }

  def allAtomicFrom(course: String, labwork: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    filteredAtomicOnly(NonSecureBlock)(rebase(labworkAttribute -> Seq(labwork)))
  }

  def createFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    create(NonSecureBlock)(rebase)
  }

  def createAtomicFrom(course: String) = restrictedContext(course)(Create) asyncContentTypedAction { implicit request =>
    createAtomic(NonSecureBlock)(rebase)
  }

  def deleteFrom(course: String, schedule: String) = restrictedContext(course)(Delete) asyncAction { implicit request =>
    delete(schedule, NonSecureBlock)(rebase(schedule))
  }

  def preview(course: String, labwork: String) = restrictedContext(course)(Create) action { implicit request =>
    import controllers.GroupCRUDController.strategyFrom

    optional2(strategyFrom(request.queryString))
      .flatMap(generate(labwork, _))
      .map(_ map toSchedule)
      .mapResult { gen =>
        Ok(Json.obj(
          "status" -> "OK",
          "schedule" -> Json.toJson(gen.elem),
          "number of conflicts" -> gen.evaluate.err.size // TODO serialize conflicts
        ))
      }
  }

  // TODO there is no preview atomic yet
  def previewAtomic(course: String, labwork: String) = restrictedContext(course)(Create) action { implicit request =>
    import controllers.GroupCRUDController.strategyFrom

    optional2(strategyFrom(request.queryString))
      .flatMap(generate(labwork, _))
      .map(_ map toSchedule)
      .mapResult { gen =>
        Ok(Json.obj(
          "status" -> "OK",
          "schedule" -> Json.toJson(gen.elem),
          "number of conflicts" -> gen.evaluate.err.size // TODO serialize conflicts
        ))
      }
  }

  override def header = Action { implicit request =>
    NoContent.as(mimeType)
  }

  private def generate[R](labwork: String, groupStrategy: Strategy)(implicit request: Request[R]): Attempt[Gen[ScheduleG, Conflict, Int]] = {
    import defaultBindings.{AssignmentPlanDescriptor, LabworkAtomDescriptor, TimetableDescriptor}

    def extract(query: Map[String, Seq[String]])(key: String) = query.get(key).flatMap(_.headOption).map(_.toInt)

    val labId = UUID.fromString(labwork)

    val genesis = for {
      lab <- repository.get[LabworkAtom](Labwork.generateUri(labId))
      semester = lab map (_.semester)
      groups <- groupService.groupBy(labId, groupStrategy)
      timetable <- repository.getAll[Timetable].map(_.find(_.labwork == labId))
      plans <- repository.getAll[AssignmentPlan].map(_.find(_.labwork == labId))
      all <- repository.getAll[ScheduleAtom]
      comp = scheduleGenesisService.competitive(lab, all)
      valueOf = extract(request.queryString) _
      pop = valueOf(popAttribute)
      gen = valueOf(genAttribute)
      elite = valueOf(eliteAttribute)
    } yield for {
      t <- timetable if t.entries.nonEmpty
      p <- plans if p.entries.nonEmpty
      s <- semester
      g <- if (groups.nonEmpty) Some(groups) else None
    } yield {
      comp.foreach(s => println(s"comp :: ${s.labwork}"))
      val genesis = scheduleGenesisService.generate(t, g, p, s, comp.toVector, pop, gen, elite)
      val newGroups = genesis._1.elem.entries.map(_.group)
      printGroup(groups, newGroups)

      println(s"final conflicts ${genesis._1.evaluate.err.size} :: fitness ${genesis._2}")
      genesis._1
    }

    optional(genesis)
  }

  def printGroup(g1: Set[Group], g2: Vector[Group]): Unit = {
    g1.toVector.sortBy(_.label).zip(g2.distinct.sortBy(_.label)).foreach {
      case (l, r) =>
        println(s" ${l.label} :: ${r.label} :: ${l.id == r.id}")
        l.members.toVector.sorted zip r.members.toVector.sorted foreach (t => println(s"${t._1} :: ${t._2} :: ${t._1 == t._2}"))
        println("=========")
    }
  }

  override protected def coAtomic(atom: ScheduleAtom): Schedule = Schedule(
    atom.labwork.id,
    atom.entries.map(a => ScheduleEntry(a.labwork.id, a.start, a.end, a.date, a.room.id, a.supervisor map (_.id), a.group.id, a.invalidated, a.id)),
    atom.invalidated,
    atom.id
  )

  override protected def getWithFilter(queryString: Map[String, Seq[String]])(all: Set[Schedule]): Try[Set[Schedule]] = {
    import store.sparql.select
    import store.sparql.select._
    import utils.Ops.MonadInstances.listM

    lazy val lwm = LWMPrefix[repository.Rdf]
    lazy val rdf = RDFPrefix[repository.Rdf]

    queryString.foldLeft(Try(**(v("schedules"), p(rdf.`type`), s(lwm.Schedule)))) {
      case (clause, (`labworkAttribute`, labworks)) => clause map {
        _ append **(v("schedules"), p(lwm.labwork), o(Labwork.generateUri(UUID.fromString(labworks.head))))
      }
      case _ => Failure(new Throwable("Unknown attribute"))
    } flatMap { clause =>
      val query = select distinct "schedules" where clause

      repository.prepareQuery(query)
        .select(_.get("schedules"))
        .transform(_.fold(List.empty[Value])(identity))
        .map(_.stringValue())
        .requestAll(repository.getMany[Schedule](_))
        .run
    }
  }

  override protected def compareModel(input: Schedule, output: Schedule): Boolean = false

  override protected def fromInput(input: Schedule, existing: Option[Schedule]): Schedule = Schedule(input.labwork, input.entries, input.invalidated, existing.fold(UUID.randomUUID)(_.id))
}