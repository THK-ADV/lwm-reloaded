package controllers

import java.util.UUID

import models._
import org.joda.time.{DateTime, Interval}
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import services._
import store.SesameRepository
import store.bind.Bindings

import scala.concurrent.Future
import scala.util.control.NonFatal
import models.LwmDateTime._

final class ApiDataController(private val repository: SesameRepository,
                              val userService: UserService,
                              val assignmentPlanService: AssignmentPlanService,
                              val courseService: CourseService,
                              val degreeService: DegreeService,
                              val labworkApplicationService: LabworkApplicationService2,
                              val labworkService: LabworkService,
                              val permissionService: PermissionService,
                              val roleService: RoleService2,
                              val roomService: RoomService,
                              val semesterService: SemesterService
                             ) extends Controller with PostgresResult {

  implicit val ns = repository.namespace
  private val bindings = Bindings[repository.Rdf](repository.namespace)

  def collisionsForCurrentLabworks() = Action {
    import bindings.{SemesterDescriptor, LabworkDescriptor, ReportCardEntryDescriptor}

    val result = for {
      semester <- repository.getAll[SesameSemester]
      currentSemester = semester.find(SesameSemester.isCurrent).get
      labworks <- repository.getAll[SesameLabwork].map(_.filter(_.semester == currentSemester.id))
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

  def multipleReportCardEntries(course: String) = Action {
    import bindings.{LabworkDescriptor, ReportCardEntryDescriptor, AssignmentPlanDescriptor}

    for {
      labworks <- repository.getAll[SesameLabwork].map(_.filter(_.course == UUID.fromString(course)))
      _ = println(labworks)
      entries <- repository.getAll[ReportCardEntry].map(_.filter(entry => labworks.exists(_.id == entry.labwork)))
      _ = println(entries.groupBy(_.labwork).keys)
      aps <- repository.getAll[SesameAssignmentPlan].map(_.filter(entry => labworks.exists(_.id == entry.labwork)))
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

  def migrateUsers = Action.async {
    import bindings.{StudentDescriptor, EmployeeDescriptor}
    import models.User.writes

    val result = for {
      _ <- userService.createSchema
      sesameStudents <- Future.fromTry(repository.getAll[SesameStudent])
      _ = println(s"sesameStudents ${sesameStudents.size}")
      sesameEmployees <- Future.fromTry(repository.getAll[SesameEmployee]).map(_.map {
        case na if na.status == "n.a" =>
          SesameEmployee(na.systemId, na.lastname, na.firstname, na.email, User.EmployeeType, None, na.id)
        case employee =>
          employee
      })
      _ = println(s"sesameEmployees ${sesameEmployees.size}")
      postgresStudents = sesameStudents.map(s =>
        DbUser(s.systemId, s.lastname, s.firstname, s.email, User.StudentType, Some(s.registrationId), Some(s.enrollment), DateTime.now.timestamp, None, s.id)
      )
      postgresEmployees = sesameEmployees.map(e =>
        DbUser(e.systemId, e.lastname, e.firstname, e.email, e.status, None, None, DateTime.now.timestamp, None, e.id)
      )
      dbUsers = postgresStudents ++ postgresEmployees
      _ = println(s"dbUsers ${dbUsers.size}")
      users <- userService.createMany(dbUsers.toList)
    } yield users.map(_.toUser)

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
      _ <- degreeService.createSchema
      sesameDegrees <- Future.fromTry(repository.getAll[SesameDegree])
      _ = println(s"sesameDegrees ${sesameDegrees.size}")
      postgresDegrees = sesameDegrees.map(s => DegreeDb(s.label, s.abbreviation, DateTime.now.timestamp, s.invalidated.map(_.timestamp), s.id))
      _ = println(s"postgresDegrees ${postgresDegrees.size}")
      degrees <- degreeService.createMany(postgresDegrees.toList)
    } yield degrees.map(_.toDegree)

    result.map { degrees =>
      Ok(Json.toJson(degrees))
    }.recover {
      case NonFatal(e) =>
        InternalServerError(Json.obj("error" -> e.getMessage))
    }
  }

  def migratePermissions = Action.async {
    import bindings.RoleDescriptor
    import models.PostgresPermission.writes

    val result = for {
      _ <- permissionService.createSchema
      sesameRoles <- Future.fromTry(repository.getAll[SesameRole])
      permissions = Permissions.all + Permissions.prime + Permissions.god
      _ = println(s"permissions ${permissions.size}")
      sesamePermissions = sesameRoles.flatMap(_.permissions)
      _ = println(s"sesamePermissions ${sesamePermissions.filterNot(s => permissions.exists(_.value == s.value))}")
      postgresPermissions = permissions.map(p => PermissionDb(p.value, ""))
      _ = println(s"postgresPermissions ${postgresPermissions.size}")
      ps <- permissionService.createMany(postgresPermissions.toList)
      _ = println(s"ps ${ps.size}")
    } yield ps.map(_.toPermission)

    result.jsonResult
  }

  def migrateRoles = Action.async {
    import bindings.RoleDescriptor
    import models.Role.writes

    val result = for {
      _ <- roleService.createSchema
      sesameRoles <- Future.fromTry(repository.getAll[SesameRole])
      _ = println(s"sesameRoles ${sesameRoles.size}")
      _ = println(s"sesamePermissions ${sesameRoles.flatMap(_.permissions).size}")
      postgresPermissions <- permissionService.get()
      _ = println(s"postgresPermissions ${postgresPermissions.size}")
      postgresRoles = sesameRoles.map { r =>
        val perms = postgresPermissions.filter(p => r.permissions.exists(_.value == p.value)).map(_.id)
        RoleDb(r.label, perms.toSet, DateTime.now.timestamp, r.invalidated.map(_.timestamp), r.id)
      }
      roles <- roleService.createMany(postgresRoles.toList)
      _ = println(s"roles ${roles.size}")
      _ = println(s"permissions ${roles.flatMap(_.permissions).size}")
    } yield roles.map(_.toRole)

    result.jsonResult
  }

  def migrateSemesters = Action.async {
    import bindings.SemesterDescriptor
    import models.LwmDateTime._

    val result = for {
      _ <- semesterService.createSchema
      sesameSemesters <- Future.fromTry(repository.getAll[SesameSemester])
      _ = println(s"sesameSemesters ${sesameSemesters.size}")
      semesterDbs = sesameSemesters.map(s =>
        SemesterDb(s.label, s.abbreviation, s.start.sqlDate, s.end.sqlDate, s.examStart.sqlDate, DateTime.now.timestamp, s.invalidated.map(_.timestamp), s.id)
      )
      _ = println(s"semesterDbs ${semesterDbs.size}")
      semester <- semesterService.createMany(semesterDbs.toList)
      _ = println(s"semester ${semester.size}")
    } yield semester.map(_.toSemester)

    result.jsonResult(PostgresSemester.writes)
  }

  def migrateCourses = Action.async {
    import bindings.CourseDescriptor

    val result = for {
      _ <- courseService.createSchema
      sesameCourses <- Future.fromTry(repository.getAll[SesameCourse])
      _ = println(s"sesameCourses ${sesameCourses.size}")
      coursesDbs = sesameCourses.map(c =>
        CourseDb(c.label, c.description, c.abbreviation, c.lecturer, c.semesterIndex, DateTime.now.timestamp, c.invalidated.map(_.timestamp), c.id)
      )
      _ = println(s"coursesDbs ${coursesDbs.size}")
      courses <- courseService.createMany(coursesDbs.toList)
      _ = println(s"courses ${courses.size}")
    } yield courses.map(_.toCourse)

    result.jsonResult(PostgresCourse.writes)
  }

  def migrateLabworks = Action.async {
    import bindings.LabworkDescriptor

    val result = for {
      _ <- labworkService.createSchema
      sesameLabworks <- Future.fromTry(repository.getAll[SesameLabwork])
      _ = println(s"sesameLabworks ${sesameLabworks.size}")
      labworkDbs = sesameLabworks.map(l =>
        LabworkDb(l.label, l.description, l.semester, l.course, l.degree, l.subscribable, l.published, DateTime.now.timestamp, l.invalidated.map(_.timestamp), l.id)
      )
      _ = println(s"labworkDbs ${labworkDbs.size}")
      labworks <- labworkService.createMany(labworkDbs.toList)
      _ = println(s"labworks ${labworks.size}")
    } yield labworks.map(_.toLabwork)

    result.jsonResult
  }

  def migrateRooms = Action.async {
    import bindings.RoomDescriptor
    import models.PostgresRoom.writes

    val result = for {
      _ <- roomService.createSchema
      sesameRooms <- Future.fromTry(repository.getAll[SesameRoom])
      _ = println(s"sesameRooms ${sesameRooms.size}")

      roomDbs = sesameRooms.map(r => RoomDb(r.label, r.description, DateTime.now.timestamp, None, r.id))
      _ = println(s"roomDbs ${roomDbs.size}")

      rooms <- roomService.createMany(roomDbs.toList)
    } yield rooms.map(_.toRoom)

    result.jsonResult
  }

  def migrateLabworkApplications = Action.async {
    import bindings.LabworkApplicationDescriptor
    import models.LabworkApplication.writes

    val result = for {
      _ <- labworkApplicationService.createSchema
      sesameLapps <- Future.fromTry(repository.getAll[SesameLabworkApplication])
      _ = println(s"sesameLapps ${sesameLapps.size}")
      lappDbs = sesameLapps.map(l =>
        LabworkApplicationDb(l.labwork, l.applicant, l.friends, l.timestamp.timestamp, DateTime.now.timestamp, l.invalidated.map(_.timestamp), l.id)
      )
      _ = println(s"lappDbs ${lappDbs.size}")
      lapps <- labworkApplicationService.createMany(lappDbs.toList)
    } yield lapps.map(_.toLabworkApplication)

    result.jsonResult
  }

  def migrateAssignmentPlans = Action.async {
    import bindings.AssignmentPlanDescriptor
    import models.AssignmentPlan.writes

    val result = for {
      _ <- assignmentPlanService.createSchema
      sesamePlans <- Future.fromTry(repository.getAll[SesameAssignmentPlan])
      _ = println(s"sesamePlans ${sesamePlans.size}")
      _ = println(s"sesamePlanEntries ${sesamePlans.flatMap(_.entries).size}")
      _ = println(s"sesamePlanEntryTypes ${sesamePlans.flatMap(_.entries.flatMap(_.types)).size}")
      planDbs = sesamePlans.map { plan =>
        val entries = plan.entries.map { e =>
          val types = e.types.map(t => PostgresAssignmentEntryType(t.entryType, t.bool, t.int))
          PostgresAssignmentEntry(e.index, e.label, types, e.duration)
        }
        AssignmentPlanDb(plan.labwork, plan.attendance, plan.mandatory, entries, DateTime.now.timestamp, plan.invalidated.map(_.timestamp), plan.id)
      }
      _ = println(s"planDbs ${planDbs.size}")
      _ = println(s"planDbsEntries ${planDbs.flatMap(_.entries).size}")
      _ = println(s"planDbsEntryTypes ${planDbs.flatMap(_.entries.flatMap(_.types)).size}")
      plans <- assignmentPlanService.createMany(planDbs.toList)
    } yield plans.map(_.toAssignmentPlan)

    result.jsonResult
  }
}