package controllers

import java.util.UUID

import models._
import org.joda.time.Interval
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import services.{DegreeService, UserService}
import store.SesameRepository
import store.bind.Bindings

import scala.concurrent.Future
import scala.util.control.NonFatal

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

  import scala.concurrent.ExecutionContext.Implicits.global

  def createSchema = Action.async {
    for {
      _ <- DegreeService.createSchema
      _ <- UserService.createSchema
    } yield Ok
  }

  def dropSchema = Action.async {
    for {
      _ <- DegreeService.dropSchema
      _ <- UserService.dropSchema
    } yield Ok
  }

  def migrateUsers = Action.async {
    import bindings.{StudentDescriptor, EmployeeDescriptor}
    import models.User.writes

    val result = for {
      sesameStudents <- Future.fromTry(repository.getAll[SesameStudent])
      _ = println(s"sesameStudents ${sesameStudents.size}")
      sesameEmployees <- Future.fromTry(repository.getAll[SesameEmployee])
      _ = println(s"sesameEmployees ${sesameEmployees.size}")
      postgresStudents = sesameStudents.map(s => PostgresStudent(s.systemId, s.lastname, s.firstname, s.email, s.registrationId, s.enrollment, s.id)).map(_.dbUser)
      postgresEmployees = sesameEmployees.foldLeft(Set.empty[DbUser]) {
        case ((list, e)) =>
          if (e.status == User.EmployeeType)
            list + PostgresEmployee(e.systemId, e.lastname, e.firstname, e.email, e.id).dbUser
          else
            list + PostgresLecturer(e.systemId, e.lastname, e.firstname, e.email, e.id).dbUser
      }
      dbUsers = postgresStudents ++ postgresEmployees
      _ = println(s"dbUsers ${dbUsers.size}")
      ok <- UserService.dropAndCreateSchema
      users <- UserService.createMany(dbUsers).map(_.map(_.user))
    } yield users.toSet

    result.map { users =>
      println(s"users ${users.size}")
      Ok(Json.toJson(users))
    }.recover {
      case NonFatal(e) =>
        InternalServerError(Json.obj("error" -> e.getMessage))
    }
  }

  def migrateDegrees = Action.async {
    import bindings.DegreeDescriptor
    import models.PostgresDegree.writes

    val result = for {
      sesameDegrees <- Future.fromTry(repository.getAll[SesameDegree])
      _ = println(s"sesameDegrees ${sesameDegrees.size}")
      postgresDegrees = sesameDegrees.map(s => PostgresDegree(s.label, s.abbreviation, s.id))
      _ = println(s"postgresDegrees ${postgresDegrees.size}")
      degrees <- DegreeService.createMany(postgresDegrees)
    } yield degrees.map(_.copy())

    result.map { degrees =>
      Ok(Json.toJson(degrees))
    }.recover {
      case NonFatal(e) =>
        InternalServerError(Json.obj("error" -> e.getMessage))
    }
  }
}