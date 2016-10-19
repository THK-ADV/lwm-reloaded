package controllers

import java.util.UUID

import models.UriGenerator
import models.labwork._
import models.semester.{Blacklist, Semester}
import org.joda.time.{DateTimeConstants, Interval}
import org.openrdf.query.QueryLanguage
import org.w3.banana.RDFPrefix
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import store.Prefixes.LWMPrefix
import store.SesameRepository
import store.bind.Bindings

import scala.util.{Failure, Success}
import scala.util.Try

class ApiDataController(private val repository: SesameRepository) extends Controller {

  implicit val ns = repository.namespace

  private val bindings = Bindings[repository.Rdf](repository.namespace)

  def patchTimetableEntry = Action { implicit request =>
    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]

    val update = s"""
      | Delete {
      | ?s <${lwm.degree}> ?v
      | } where {
      | ?s <${rdf.`type`}> <${lwm.TimetableEntry}> .
      | ?s <${lwm.degree}> ?v
      | }
    """.stripMargin

    import repository.rdfStore.sparqlEngineSyntax._
    val check =
      s"""
         |Select distinct ?v where {
         | ?s <${rdf.typ}> <${lwm.TimetableEntry}> .
         | ?s <${lwm.degree}> ?v
         |}
       """.stripMargin
    def lookAt = repository.connect(conn => repository.sparqlOps.parseSelect(check) flatMap (s => conn.executeSelect(s)))

    lookAt map (s =>  println(s map (_.getValue("v"))))
    repository.connect(c => Try(c.prepareUpdate(QueryLanguage.SPARQL, update).execute()))
    lookAt map (s =>  println(s map (_.getValue("v"))))

    Ok
  }

  def removeBlacklists() = Action { implicit request =>
    import bindings.BlacklistDescriptor

    val result = repository.getAll[Blacklist] flatMap { blacklists =>
      repository.deleteMany[Blacklist](blacklists map Blacklist.generateUri)
    }

    result match {
      case Success(s) => Ok(Json.obj(
        "status" -> "OK",
        "message" -> s"deleted ${s.size} elements"
      ))
      case Failure(e) => InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }

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

  def patchEntries(labwork: String) = Action { implicit request =>
    import bindings.{ScheduleDescriptor, ReportCardEntryDescriptor}

    val result = for {
      schedule <- repository.getAll[Schedule]
      reportCard <- repository.getAll[ReportCardEntry]
      ktnSchedule = schedule.filter(_.labwork == UUID.fromString(labwork))
      ktnReportCards = reportCard.filter(e => e.labwork == UUID.fromString(labwork) && (e.date.getDayOfWeek == DateTimeConstants.WEDNESDAY && e.start.getHourOfDay == 14))
      scheduleApplied = ktnSchedule.map(s => Schedule(s.labwork, s.entries.map {
        case wednesday if wednesday.date.getDayOfWeek == DateTimeConstants.WEDNESDAY && wednesday.start.getHourOfDay == 14 =>
          ScheduleEntry(wednesday.labwork, wednesday.start.minusHours(3), wednesday.end.minusHours(3), wednesday.date, wednesday.room, wednesday.supervisor, wednesday.group, wednesday.invalidated, wednesday.id)
        case other => other
      }, s.invalidated, s.id))
      reportCardApplied = ktnReportCards.map(c => ReportCardEntry(c.student, c.labwork, c.label, c.date, c.start.minusHours(3), c.end.minusHours(3), c.room, c.entryTypes, c.rescheduled, c.invalidated, c.id))
      updateScheduleOk = scheduleApplied.map(s => repository.update[Schedule, UriGenerator[Schedule]](s)(ScheduleDescriptor, Schedule))
      updateReportCardOk = reportCardApplied.map(r => repository.update[ReportCardEntry, UriGenerator[ReportCardEntry]](r)(ReportCardEntryDescriptor, ReportCardEntry))
    } yield updateScheduleOk.map(_.isSuccess).reduce(_ && _) && updateReportCardOk.map(_.isSuccess).reduce(_ && _)

    result match {
      case Success(s) => Ok(Json.obj(
        "status" -> "OK",
        "message" -> s
      ))
      case Failure(e) => InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }
}