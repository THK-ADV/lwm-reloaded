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
      _ <- if (preview.toBoolean) Success(ValueFactoryImpl.getInstance().createLiteral("")) else repository.update(newTimetable)(TimetableDescriptor, Timetable)
      scheduleEntries <- repository.getAll[ScheduleEntry].map(_.filter(_.labwork == UUID.fromString(labwork)))
      newScheduleEntries = scheduleEntries.map(e => e.copy(e.labwork, e.start, e.end, e.date, e.room, e.supervisor + supervisorId))
      _ <- if (preview.toBoolean) Success(ValueFactoryImpl.getInstance().createLiteral("")) else newScheduleEntries.map(e => repository.update(e)(ScheduleEntryDescriptor, ScheduleEntry)).sequence
    } yield (newTimetable.entries, newScheduleEntries)

    entries match {
      case Success(s) => Ok(Json.obj(
        "status" -> "OK",
        "timetableEntries" -> Json.toJson(s._1),
        "scheduleEntries" -> Json.toJson(s._2)
      ))
      case Failure(e) => InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }
}