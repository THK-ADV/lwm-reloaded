package controllers

import java.util.UUID

import models._
import org.joda.time.{Interval, LocalDateTime}
import org.openrdf.model.impl.ValueFactoryImpl
import org.specs2.json.JSONObject
import play.api.libs.json._
import play.api.mvc.{AnyContent, Controller, Request}
import services.{RoleService, SessionHandlingService}
import store.SesameRepository
import store.bind.Bindings
import utils.LwmMimeType

import scala.util.{Failure, Success, Try}

class ApiDataController(
  val repository: SesameRepository,
  val sessionService: SessionHandlingService,
  val roleService: RoleService
) extends Controller with Secured with SessionChecking with SecureControllerContext with ContentTyped {

  implicit val ns = repository.namespace
  private val bindings = Bindings[repository.Rdf](repository.namespace)

  override implicit def mimeType = LwmMimeType.apiDataV1Json

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case _ => PartialSecureBlock(Permissions.prime)
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, Permissions.reportCardEvaluation.create)
    case _ => PartialSecureBlock(Permissions.god)
  }

  def collisionsForCurrentLabworks(semesterIndex: String) = contextFrom(Get) action { implicit request =>
    import bindings.{SemesterDescriptor, LabworkAtomDescriptor, ReportCardEntryDescriptor}
    import models.ReportCardEntry.writes

    val result = for {
      semester <- repository.getAll[Semester]
      currentSemester = semester.find(Semester.isCurrent).get
      index = semesterIndex.toInt
      labworks <- repository.getAll[LabworkAtom].map(_.filter(l => l.semester.id == currentSemester.id && (if (index == -1) true else l.course.semesterIndex == index)))
      _ = println(labworks.map(_.label))
      cards <- repository.getAll[ReportCardEntry].map(_.filter(c => labworks.exists(_.id == c.labwork)))
      byStudents = cards.groupBy(_.student)
    } yield byStudents

    result match {
      case Success(s) =>
          val entries = s.mapValues { reportCards =>
            val (collision, cards) = reportCards.foldLeft((true, Set.empty[(ReportCardEntry, ReportCardEntry)])) { // by this way, Pair(A, B) and Pair(B, A) are both in set
              case ((outerCollision, outerCards), r) =>
                val sameDateAs = reportCards.filter(r2 => r2.date.isEqual(r.date) && r2.id != r.id)

                val (innerCollision, cards) = sameDateAs.foldLeft((true, Set.empty[(ReportCardEntry, ReportCardEntry)])) { // false means collision
                  case ((bool, set), next) =>
                    val baseInterval = new Interval(r.date.toDateTime(r.start), r.date.toDateTime(r.end))
                    val nextInterval = new Interval(next.date.toDateTime(next.start), next.date.toDateTime(next.end))

                    val overlaps = baseInterval overlaps nextInterval
                    val acc = bool && !overlaps
                    val appended = if (overlaps) set.+((r, next)) else set

                    (acc, appended)
                }

                (innerCollision && outerCollision, outerCards ++ cards)
            }

            (!collision, cards)
          }

          val collisions = entries.filter(_._2._1)

        Ok(Json.obj(
          "status" -> "OK",
          "collision count" -> collisions.size,
          "collisions" -> collisions.map {
            case (student, (bool, reportCards)) => Json.obj(
              "student" -> student,
              "collision" -> bool,
              "colliding reportCards" -> reportCards.map {
                case (l, r) => Json.obj(
                  "first" -> Json.toJson(l),
                  "second" -> Json.toJson(r)
                )
              }
            )
          },
          "no collisions" -> entries.map {
            case (student, (bool, reportCards)) => Json.obj(
              "student" -> student,
              "collision" -> bool,
              "colliding reportCards" -> reportCards.map {
                case (l, r) => Json.obj(
                  "first" -> Json.toJson(l),
                  "second" -> Json.toJson(r)
                )
              }
            )
          }
        ))
      case Failure(e) =>
        InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }

  def multipleReportCardEntries(course: String) = contextFrom(Get) action { implicit request =>
    import bindings.{LabworkDescriptor, ReportCardEntryDescriptor, AssignmentPlanDescriptor}

    for {
      labworks <- repository.getAll[Labwork].map(_.filter(_.course == UUID.fromString(course)))
      _ = println(labworks)
      entries <- repository.getAll[ReportCardEntry].map(_.filter(entry => labworks.exists(_.id == entry.labwork)))
      _ = println(entries.groupBy(_.labwork).keys)
      aps <- repository.getAll[AssignmentPlan].map(_.filter(entry => labworks.exists(_.id == entry.labwork)))
      grouped = entries.groupBy(_.student)
      _ = grouped.foreach {
        case (student, reportCardEntries) if reportCardEntries.size > aps.find(_.labwork == reportCardEntries.head.labwork).get.entries.size => println(s"student $student with ${reportCardEntries.size} entries")
        case (_, reportCardEntries) if reportCardEntries.size == aps.find(_.labwork == reportCardEntries.head.labwork).get.entries.size =>
        case _ => println("oops")
      }
    } yield 1

    Ok
  }

  case class ReportCardEntryProtocol(label: String, date: String, start: String, end: String, room: UUID, entryTypes: Set[ReportCardEntryTypeProtocol])
  case class ReportCardEntryTypeProtocol(entryType: String, bool: Boolean, int: Int)

  def appendReportCardEntries(labwork: String, preview: String) = contextFrom(Update) contentTypedAction { implicit request =>
    import models.LwmDateTime._
    import bindings.{ReportCardEntryDescriptor, LabworkDescriptor}
    import models.ReportCardEntry._

    println(request.body)

    implicit def readsType = Json.reads[ReportCardEntryTypeProtocol]
    implicit def readsEntry = Json.reads[ReportCardEntryProtocol]

    val reportCardEntries = for {
      newCards <- request.body.validate[Array[ReportCardEntryProtocol]].fold(
        errors => Failure(new Throwable(JsError.toJson(errors).toString)),
        reportCards => Success(reportCards)
      )
      labworkId = UUID.fromString(labwork)
      existingCards <- repository.getAll[ReportCardEntry].map(_.filter(_.labwork == labworkId))
      _ = println(s"existingCards count ${existingCards.size}")
      newReportCardEntries = existingCards.groupBy(_.student).keys.flatMap { student =>
        newCards.map { c =>
          ReportCardEntry(student, labworkId, c.label, toLocalDate(c.date), toLocalTime(c.start), toLocalTime(c.end), c.room, c.entryTypes.map(t => ReportCardEntryType(t.entryType, t.bool, t.int)))
        }.toSet
      }.toSet
      _ = println(s"newReportCardEntries count ${newReportCardEntries.size}")
      created <- if (preview.toBoolean) Success(newReportCardEntries) else repository.addMany(newReportCardEntries).map(_ => newReportCardEntries)
    } yield (created ++ existingCards).groupBy(_.student)

    reportCardEntries match {
      case Success(s) => Ok(Json.obj(
        "status" -> "OK",
        "students with cards" -> s.map {
          case (student, cards) => Json.obj(
            "student" -> student.toString,
            "reportCardEntries" -> Json.toJson(cards)
          )
        }
      ))
      case Failure(e) => InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }

  val fakeSuccessGraph = Success(ValueFactoryImpl.getInstance().createLiteral(""))

  def appendSupervisorToScheduleEntries(supervisor: String, labwork: String, preview: String) = contextFrom(Update) action { implicit request =>
    import utils.Ops._
    import utils.Ops.MonadInstances.tryM
    import bindings.{TimetableDescriptor, ScheduleEntryDescriptor}
    import Timetable.writes
    import ScheduleEntry.writes

    val supervisorId = UUID.fromString(supervisor)

    val entries = for {
      timetable <- repository.getAll[Timetable].map(_.find(_.labwork == UUID.fromString(labwork)).head)
      newTimetable = timetable.copy(timetable.labwork, timetable.entries.map(e => e.copy(e.supervisor + supervisorId)))
      _ <- if (preview.toBoolean) fakeSuccessGraph else repository.update(newTimetable)(TimetableDescriptor, Timetable)
      scheduleEntries <- repository.getAll[ScheduleEntry].map(_.filter(_.labwork == UUID.fromString(labwork)))
      newScheduleEntries = scheduleEntries.map(e => e.copy(e.labwork, e.start, e.end, e.date, e.room, e.supervisor + supervisorId))
      _ <- if (preview.toBoolean) fakeSuccessGraph else newScheduleEntries.map(e => repository.update(e)(ScheduleEntryDescriptor, ScheduleEntry)).sequence
    } yield (newTimetable.entries, newScheduleEntries)

    entries match {
      case Success((timetableEntries, scheduleEntries)) => Ok(Json.obj(
        "status" -> "OK",
        "timetableEntries" -> Json.toJson(timetableEntries),
        "scheduleEntries" -> Json.toJson(scheduleEntries)
      ))
      case Failure(e) => InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }

  // TODO expand group swap request and refactor to groupService at some time
  case class GroupSwapRequest(srcLabwork: UUID, srcStudent: String, destGroup: String)

  def swapGroup(preview: String) = contextFrom(Update) contentTypedAction { implicit request =>
    import bindings.{GroupDescriptor, ReportCardEntryDescriptor, StudentDescriptor}
    import utils.Ops._
    import utils.Ops.MonadInstances.tryM
    import models.LwmDateTime._
    import models.Group._
    import models.ReportCardEntry._

    def replaceMembersIn(group: Group)(updateMembers: Set[UUID] => Set[UUID]): Group = {
      group.copy(group.label, group.labwork, updateMembers(group.members))
    }

    implicit val reads = Json.reads[GroupSwapRequest]

    val swappedGroups = for {
      swapRequest <- request.body.validate[GroupSwapRequest].fold(
        errors => Failure(new Throwable(JsError.toJson(errors).toString)),
        swapRequest => Success(swapRequest)
      )
      groups <- repository.getAll[Group].map(_.filter(_.labwork == swapRequest.srcLabwork))
      srcStudent <- repository.getAll[Student].map(_.find(_.systemId == swapRequest.srcStudent).head).map(_.id)
      currentGroup = groups.find(_.members.contains(srcStudent)).get
      destinationGroup = groups.find(_.label == swapRequest.destGroup).get
      destinationStudent = destinationGroup.members.head
      updatedCurrentGroup = replaceMembersIn(currentGroup)(_ - srcStudent)
      updatedDestinationGroup = replaceMembersIn(destinationGroup)(_ + srcStudent)
      _ <- if (preview.toBoolean)
        fakeSuccessGraph
      else
        List(updatedCurrentGroup, updatedDestinationGroup).map(g => repository.update(g)(GroupDescriptor, Group)).sequence
      reportCardEntries <- repository.getAll[ReportCardEntry].map(_.filter(e => e.labwork == swapRequest.srcLabwork))
      studentEntries = reportCardEntries.filter(_.student == srcStudent).toList.sortBy(r => r.date.toLocalDateTime(r.start))
      templateEntries = reportCardEntries.filter(_.student == destinationStudent).toList.sortBy(r => r.date.toLocalDateTime(r.start))
      updatedStudentEntries = for {
        (origin, template) <- studentEntries.zip(templateEntries) if /*origin.date.getWeekOfWeekyear == template.date.getWeekOfWeekyear &&*/ origin.label == template.label
      } yield origin.copy(origin.student, origin.labwork, origin.label, template.date, template.start, template.end, template.room)
      _ <- if (preview.toBoolean)
        fakeSuccessGraph
      else
        updatedStudentEntries.map(r => repository.update(r)(ReportCardEntryDescriptor, ReportCardEntry)).sequence
    } yield (updatedCurrentGroup, updatedDestinationGroup, studentEntries, updatedStudentEntries)

    swappedGroups match {
      case Success((oldGroup, newGroup, oldCards, newCards)) => Ok(Json.obj(
        "status" -> "OK",
        "oldGroup" -> Json.toJson(oldGroup),
        "newGroup" -> Json.toJson(newGroup),
        "oldCards" -> Json.toJson(oldCards),
        "newCards" -> Json.toJson(newCards)
      ))
      case Failure(e) => InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }

  case class ScheduleSupervisorSwapRequest(srcSupervisor: UUID, destSupervisor: UUID, groupLabel: String)
  // this implementation assumes that one supervisor consists on his groups
  def swapSupervisor(scheduleId: String, preview: String) = contextFrom(Update) contentTypedAction { implicit request =>
    import utils.Ops._
    import utils.Ops.MonadInstances.tryM
    import bindings.{ScheduleDescriptor, GroupDescriptor, ScheduleEntryDescriptor}
    import models.ScheduleEntry._

    implicit val reads = Json.reads[ScheduleSupervisorSwapRequest]

    val scheduleEntries = for {
      swapRequest <- request.body.validate[ScheduleSupervisorSwapRequest].fold(
        errors => Failure(new Throwable(JsError.toJson(errors).toString)),
        swapRequest => Success(swapRequest)
      )
      schedule <- repository.get[Schedule](Schedule.generateUri(UUID.fromString(scheduleId))).map(_.get)
      group <- repository.getAll[Group].map(_.find(g => g.labwork == schedule.labwork && g.label == swapRequest.groupLabel).get)
      scheduleEntries = schedule.entries.filter(_.group == group.id).map { entry =>
        entry.copy(entry.labwork, entry.start, entry.end, entry.date, entry.room, (entry.supervisor - swapRequest.srcSupervisor) + swapRequest.destSupervisor)
      }
      _ = println(s"new ${scheduleEntries.map(e => e.date.toLocalDateTime(e.start))}")
      _ <- if (preview.toBoolean) fakeSuccessGraph else scheduleEntries.map(e => repository.update(e)(ScheduleEntryDescriptor, ScheduleEntry)).sequence
    } yield scheduleEntries

    scheduleEntries match {
      case Success(s) => Ok(Json.obj(
        "status" -> "OK",
        "entries" -> Json.toJson(s)
      ))
      case Failure(e) => InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }

  case class SwapReportCardAssignmentRequest(labwork: String, firstIndex: Int, secondIndex: Int, appointments: Int, firstLabelAssumption: String, secondLabelAssumption: String)
  // this implementation assumes that reportCardEntries are sorted by date and start in order to be accessible by an index
  def swapReportCardAssignments(preview: String) = contextFrom(Update) contentTypedAction { implicit request =>
    import bindings.ReportCardEntryDescriptor
    import models.LwmDateTime._
    import models.ReportCardEntry._

    def assertProperSorting(groupedByStudent: Map[UUID, List[ReportCardEntry]]): Unit = {
      val sortedProperly = groupedByStudent.values.toList.forall { entries =>
        entries.drop(1).foldLeft((true, entries.head)) {
          case ((bool, prev), curr) =>
            println(prev.date.toLocalDateTime(prev.start))
            val properly = prev.date.toLocalDateTime(prev.start).isBefore(curr.date.toLocalDateTime(curr.start))
            (properly && bool, curr)
        }._1
      }

      assert(sortedProperly, "Oops, sorting failed")
    }

    def sort(entries: List[ReportCardEntry]) = {
      entries.sortBy(e => e.date.toLocalDateTime(e.start))
    }

    def copy(first: ReportCardEntry, second: ReportCardEntry) = {
      ReportCardEntry(
        first.student,
        first.labwork,
        second.label,
        first.date,
        first.start,
        first.end,
        first.room,
        second.entryTypes,
        first.rescheduled,
        first.invalidated,
        first.id
      )
    }

    implicit val reads = Json.reads[SwapReportCardAssignmentRequest]

    val swappedReportCards = for {
      swapRequest <- request.body.validate[SwapReportCardAssignmentRequest].fold(
        errors => Failure(new Throwable(JsError.toJson(errors).toString)),
        swapRequest => Success(swapRequest)
      )
      reportCards <- repository.getAll[ReportCardEntry].map(_.filter(_.labwork == UUID.fromString(swapRequest.labwork)))
      groupedByStudent = reportCards.toList.groupBy(_.student).mapValues(e => sort(e).take(swapRequest.appointments))
      firstLabelAssumption = swapRequest.firstLabelAssumption
      secondLabelAssumption = swapRequest.secondLabelAssumption
      _ = assertProperSorting(groupedByStudent)
      swappedReportCards = groupedByStudent.mapValues { entries =>
        val first = entries(swapRequest.firstIndex)
        val second = entries(swapRequest.secondIndex)

        assert(first.label == firstLabelAssumption, s"failed firstLabelAssumption $firstLabelAssumption with ${first.label}")
        assert(second.label == secondLabelAssumption, s"failed secondLabelAssumption $secondLabelAssumption with ${second.label}")

        val swappedEntries = List(copy(first, second), copy(second, first))

        if (!preview.toBoolean) {
          val succeeded = swappedEntries.map(e => repository.update(e)(ReportCardEntryDescriptor, ReportCardEntry)).forall(_.isSuccess)
          assert(succeeded, "Oops, repository.update failed")
        }

        val all = sort(swappedEntries ::: entries.filterNot(e => e.id == first.id || e.id == second.id))
        assert(all.size == swapRequest.appointments, "Oops, wrong size")
        assert(all.groupBy(_.id).values.forall(_.size == 1), "Oops, size is broken")

        all
      }
      _ = assertProperSorting(swappedReportCards)
    } yield swappedReportCards

    swappedReportCards match {
      case Success(s) => Ok(Json.obj(
        "status" -> "OK",
        "swapped reportCards" -> s.map {
          case (student, entries) => Json.obj(
            "student" -> student,
            "entries" -> Json.toJson(entries)
          )
        }
      ))
      case Failure(e) => InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }

  def bumpBonus(course: String, preview: String) = contextFrom(Update) action { implicit request =>
    import bindings.{LabworkDescriptor, ReportCardEntryDescriptor, ReportCardEntryTypeDescriptor, SemesterDescriptor}
    import utils.Ops._
    import utils.Ops.MonadInstances.tryM
    import models.ReportCardEntryType._

    val result = for {
      semesters <- repository.getAll[Semester].map(_.find(Semester.isCurrent))
      labworks <- repository.getAll[Labwork].map(_.filter(l => l.course == UUID.fromString(course) && semesters.exists(_.id == l.semester)))
      reportCards <- repository.getAll[ReportCardEntry].map(_.filter(l => labworks.exists(_.id == l.labwork)))
      _ = println(s"students ${reportCards.groupBy(_.student).keys.size}")
      assignment = reportCards.filter(e => e.label == "Pflichtteil 1 - Server")
      _ = println(s"assignment ${assignment.size}")
      passedAssignment = assignment.filter(_.entryTypes.exists(t => t.entryType == ReportCardEntryType.Certificate.entryType && t.bool))
      _ = println(s"passedAssignment ${passedAssignment.size}")
      bumped = passedAssignment.map(e => e.entryTypes.find(_.entryType == ReportCardEntryType.Bonus.entryType).get.copy(int = 50))
      _ = println(s"bumped ${bumped.size}")
      _ <- if (preview.toBoolean) fakeSuccessGraph else bumped.map(e => repository.update(e)(ReportCardEntryTypeDescriptor, ReportCardEntryType)).sequence
    } yield bumped

    result match {
      case Success(s) => Ok(Json.obj(
        "status" -> "OK",
        "bumped" -> Json.toJson(s)
        ))
      case Failure(e) => InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }

  def assignmentStatistic(course: String) = contextFrom(Get) action { implicit request =>
    import bindings.{LabworkAtomDescriptor, ReportCardEntryDescriptor, SemesterDescriptor}

    def percentage(of: Int, total: Int) = (100 * of) / total
    def entryTypeJson(of: Int, total: Int) = Json.obj(
      "absolute" -> of,
      "percentage" -> percentage(of, total)
    )

    val statistic = for {
      semesters <- repository.getAll[Semester].map(_.find(Semester.isCurrent))
      labworks <- repository.getAll[LabworkAtom].map(_.filter(l => l.course.id == UUID.fromString(course) && semesters.exists(_.id == l.semester.id)))
      reportCards <- repository.getAll[ReportCardEntry].map(_.filter(l => labworks.exists(_.id == l.labwork)))
      grouping = reportCards.groupBy(_.labwork)
    } yield (semesters.head, labworks, grouping)

    statistic match {
      case Success((semester, labworks, grouping)) => Ok(Json.obj(
        "status" -> "OK",
        "semester" -> semester.label,
        "statistic" -> grouping.map {
          case (labwork, reportCards) =>
            val assignments = reportCards.groupBy(_.label).mapValues(_.flatMap(_.entryTypes)) // TODO ordering
            val students = reportCards.groupBy(_.student).size

            Json.obj(
              "labwork" -> labworks.find(_.id == labwork).get.label,
              "students" -> students,
              "assignments" -> assignments.map {
                case (label, entryTypes) =>
                  val attendance = entryTypes.count(e => e.entryType == ReportCardEntryType.Attendance.entryType && e.bool)
                  val testat = entryTypes.count(e => e.entryType == ReportCardEntryType.Certificate.entryType && e.bool)
                  val points = entryTypes.count(e => e.entryType == ReportCardEntryType.Bonus.entryType && e.int > 0)

                  Json.obj(
                    "label" -> label,
                    "attendance" -> entryTypeJson(attendance, students),
                    "testat" -> entryTypeJson(testat, students),
                    "points" -> entryTypeJson(points, students)
                )
              }
          )
        }
      ))
      case Failure(e) => InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }

  def bonicheck(course: String) = contextFrom(Get) action { implicit request =>
    import bindings.{LabworkDescriptor, ReportCardEntryAtomDescriptor, SemesterDescriptor}
    import models.Student.writes

    val eval = for {
      semesters <- repository.getAll[Semester].map(_.find(Semester.isCurrent))
      labworks <- repository.getAll[Labwork].map(_.filter(l => l.course == UUID.fromString(course) && semesters.exists(_.id == l.semester)))
      reportCards <- repository.getAll[ReportCardEntryAtom].map(_.filter(l => labworks.exists(_.id == l.labwork.id)))
      grouping = reportCards.groupBy(_.student)
    } yield grouping

    eval match {
      case Success(s) => Ok(Json.toJson(s.map {
        case (student, cards) =>

          val missingBonusPoints = cards.foldLeft(Set.empty[Student]) {
            case (students, c) =>
              val cert = c.entryTypes.exists(t => t.entryType == ReportCardEntryType.Certificate.entryType && t.bool)
              val noBonus = c.entryTypes.exists(t => t.entryType == ReportCardEntryType.Bonus.entryType && t.int == 0)

              if (cert && noBonus) students + student else students
          }

          Json.toJson(missingBonusPoints)
      }))
      case Failure(e) => InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }

  def assignmentEvaluation(course: String) = restrictedContext(course)(Create) action { implicit request =>
    val courseId = UUID.fromString(course)

    assignmentEvals(_.course == courseId)
  }

  def assignmentEvaluationA(course: String, labwork: String) = restrictedContext(course)(Create) action { implicit request =>
    val courseId = UUID.fromString(course)
    val labworkId = UUID.fromString(labwork)

    assignmentEvals(l => l.id == labworkId && l.course == courseId)
  }

  private def assignmentEvals(consider: Labwork => Boolean)(implicit request: Request[AnyContent]) = {
    import bindings.{LabworkDescriptor, ReportCardEntryAtomDescriptor, SemesterDescriptor}
    import models.LwmDateTime._

    val eval = for {
      semesters <- repository.getAll[Semester].map(_.find(Semester.isCurrent)) if semesters.size == 1
      currentSemester = semesters.head.id
      labworks <- repository.getAll[Labwork].map(_.filter(l => consider(l) && l.semester == currentSemester))
      reportCards <- repository.getAll[ReportCardEntryAtom].map(_.filter(l => labworks.exists(_.id == l.labwork.id)))
      grouping = reportCards.groupBy(_.student)
    } yield grouping

    eval match {
      case Success(s) => Ok(Json.toJson(s.map {
        case (student, cards) =>
          val entryTypes = cards.flatMap(_.entryTypes)

          val points = entryTypes.filter(_.entryType == ReportCardEntryType.Bonus.entryType).foldLeft(0) {
            case (sum, entry) => sum + entry.int
          }
          val certificates = entryTypes.filter(_.entryType == ReportCardEntryType.Certificate.entryType).count(_.bool)
          val attendances = entryTypes.filter(_.entryType == ReportCardEntryType.Attendance.entryType).count(_.bool)

          val assignments = cards.toList.sortBy(e => e.date.toLocalDateTime(e.start)).foldLeft(Seq.empty[(String, JsValue)]) {
            case (seq, entry) =>
              val cert = entry.entryTypes.find(_.entryType == ReportCardEntryType.Certificate.entryType).map(_.bool)
              val points = entry.entryTypes.find(_.entryType == ReportCardEntryType.Bonus.entryType).map(_.int)

              val subJson = (cert, points) match {
                case (Some(crt), Some(pts)) => Some(Json.obj(
                  "Testat" -> JsBoolean(crt),
                  "Punkte" -> JsNumber(pts)
                ))
                case (Some(crt), None) => Some(Json.obj("Testat" -> JsBoolean(crt)))
                case (None, Some(pts)) => Some(Json.obj("Punkte" -> JsNumber(pts)))
                case (None, None) => None
              }

              subJson.fold(seq)(js => seq.:+(entry.label -> js))
          }

          val a = ReportCardEntryType.Attendance.entryType.toLowerCase
          val b = ReportCardEntryType.Bonus.entryType.toLowerCase
          val c = ReportCardEntryType.Certificate.entryType.toLowerCase

          val passed = request.queryString.foldLeft(Map.empty[String, JsValue]) {
            case (map, (key, values)) =>
              val passed = Try(values.head.toInt).map { int =>
                val score = key.toLowerCase match {
                  case `a` => attendances
                  case `b` => points
                  case `c` => certificates
                  case _ => 0
                }

                JsBoolean(score >= int)
              }

              passed match {
                case Success(jsValue) => map + ((s"Bestanden nach $key", jsValue))
                case Failure(_) => map
              }
          }

          Json.obj(
            "Datum" -> LocalDateTime.now.toString,
            "Praktikum" -> cards.head.labwork.label,
            "Nachname" -> student.lastname,
            "Vorname" -> student.firstname,
            "Email" -> student.email,
            "GMID" -> student.systemId,
            "Matrikelnummer" -> student.registrationId,
            "Punkte" -> points,
            "Anwesenheiten" -> attendances,
            "Testate" -> certificates,
            "Auswertung" -> passed,
            "Aufgaben" -> JsObject(assignments)
          )
      }))
      case Failure(e) => InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }
}