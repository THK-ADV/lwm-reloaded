package controllers

import java.util.UUID

import models._
import org.joda.time.Interval
import org.openrdf.model.impl.ValueFactoryImpl
import play.api.libs.json.{JsError, JsValue, Json}
import play.api.mvc.{Action, BodyParsers, Controller}
import store.SesameRepository
import store.bind.Bindings

import scala.util.{Failure, Success, Try}

class ApiDataController(private val repository: SesameRepository) extends Controller {

  implicit val ns = repository.namespace
  private val bindings = Bindings[repository.Rdf](repository.namespace)

  def collisionsForCurrentLabworks() = Action { request =>
    import bindings.{SemesterDescriptor, LabworkDescriptor, ReportCardEntryDescriptor}

    val result = for {
      semester <- repository.getAll[Semester]
      currentSemester = semester.find(Semester.isCurrent).get
      labworks <- repository.getAll[Labwork].map(_.filter(_.semester == currentSemester.id))
      cards <- repository.getAll[ReportCardEntry].map(_.filter(c => labworks.exists(_.id == c.labwork)))
      byStudents = cards.groupBy(_.student)
    } yield byStudents.mapValues(e => e.map(ee => new Interval(ee.date.toDateTime(ee.start), ee.date.toDateTime(ee.end))))

    result.get.reduce { (left, right) =>
      val overlaps = left._2.forall(i => right._2.forall(ii => i.overlaps(ii)))
      if (overlaps) println("bad")
      left
    }

    Ok
  }

  def multipleReportCardEntries(course: String) = Action { request =>
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

  def appendReportCardEntries(labwork: String, preview: String) = Action(parse.json) { request =>
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
      existingCards <- repository.getAll[ReportCardEntry].map(_.filter(_.labwork == UUID.fromString(labwork)))
      _ = println(s"existingCards count ${existingCards.size}")
      newReportCardEntries = existingCards.groupBy(_.student).flatMap {
        case (student, cards) =>
          val labwork = cards.head.labwork

          newCards.map { c =>
            ReportCardEntry(student, labwork, c.label, toLocalDate(c.date), toLocalTime(c.start), toLocalTime(c.end), c.room, c.entryTypes.map(t => ReportCardEntryType(t.entryType, t.bool, t.int)))
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

  def appendSupervisorToScheduleEntries(supervisor: String, labwork: String, preview: String) = Action { request =>
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

  def swapGroup(preview: String) = Action(parse.json) { request =>
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
        (origin, template) <- studentEntries.zip(templateEntries) if origin.date.getWeekOfWeekyear == template.date.getWeekOfWeekyear && origin.label == template.label
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

  def swapSupervisor(scheduleId: String, preview: String) = Action(parse.json) { request =>
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
}