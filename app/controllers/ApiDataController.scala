package controllers

import models.{AssignmentEntry, Labwork, Course, Degree, AssignmentPlan, EntryType, Room}
import models.applications.LabworkApplication
import models.security.{Authority, RefRole, Role}
import models.security.Permissions._
import models.semester.Semester
import models.users.{Student, Employee}
import org.joda.time.LocalDate
import org.w3.banana.PointedGraph
import play.api.libs.json.{JsArray, Json}
import play.api.mvc.{Action, Controller}
import store.SesameRepository
import store.bind.Bindings
import scala.language.implicitConversions
import scala.util.Random._
import scala.util.{Failure, Success, Try}

object ApiDataController {
  val mvRole = Role("Modulverantwortlicher",
    labwork.all ++ schedule.all ++ timetable.all ++ group.all + course.update
  )
  val maRole = Role("Modulmitarbeiter",
    Set(labwork.get, labwork.getAll) ++ Set(schedule.get, schedule.getAll) ++ Set(timetable.get, timetable.getAll) + group.get
  )
  val assistantRole = Role("Hilfskraft",
    Set(schedule.get, timetable.get)
  )

  val rvRole = Role("Rechteverantwortlicher", authority.all ++ refRole.all ++ role.all)
  val studentRole = Role("Student",
    Set(room.get, degree.get, course.get, labwork.get) ++
      Set(labworkApplication.create, labworkApplication.update, labworkApplication.delete, labworkApplication.get) +
      semester.get + group.get + user.get
  )
  val employeeRole = Role("Mitarbeiter",
    room.all ++ semester.all ++ degree.all ++ user.all + blacklist.get ++ Set(course.get, course.getAll) + labworkApplication.getAll ++ entryType.all
  )
  val adminRole = Role("Admin", Set(prime))
}

class ApiDataController(val repository: SesameRepository) extends Controller {
  import repository.ops
  import ApiDataController._

  private val bindings = Bindings(repository.namespace)
  implicit def toLocalDate(s: String): LocalDate = LocalDate.parse(s)

  val adminRefRole = RefRole(None, adminRole.id)
  val employeeRefRole = RefRole(None, employeeRole.id)
  val studentRefRole = RefRole(None, studentRole.id)
  val rvRefRole = RefRole(None, rvRole.id)

  val konen = Employee.randomUUID
  val leopold = Employee.randomUUID
  val schmitter = Employee.randomUUID
  val victor = Employee.randomUUID
  val eisemann = Employee.randomUUID
  val kohls = Employee.randomUUID

  val ai = Degree.randomUUID
  val mi = Degree.randomUUID
  val wi = Degree.randomUUID
  val ti = Degree.randomUUID

  val ss15 = Semester.randomUUID
  val ws1516 = Semester.randomUUID
  val ss16 = Semester.randomUUID

  val ma1Konen = Course.randomUUID
  val ma1Leopold = Course.randomUUID
  val ma2Schmitter = Course.randomUUID
  val ap1Victor = Course.randomUUID
  val ap2Kohls = Course.randomUUID
  val cgaEisemann = Course.randomUUID

  val ap1MiPrak = Labwork.randomUUID

  val alexStudent = Student.randomUUID
  val uweStudent = Student.randomUUID
  val robertStudent = Student.randomUUID
  val christianStudent = Student.randomUUID

  val ap1Plan = {
    val amount = 8
    val entries = (0 until amount).map(n => AssignmentEntry(n, Set(EntryType("mandatory")))).toSet
    AssignmentPlan(amount, entries)
  }
  val ap2Plan = {
    val amount = 8
    val entries = (0 until amount).map(n => AssignmentEntry(n, Set(EntryType("mandatory")))).toSet
    AssignmentPlan(amount, entries)
  }
  val ma1Plan = {
    val amount = 4
    val entries = (0 until amount).map(n => AssignmentEntry(n, Set(EntryType("mandatory")))).toSet
    AssignmentPlan(amount, entries)
  }
  val ma2Plan = {
    val amount = 4
    val entries = (0 until amount).map(n => AssignmentEntry(n, Set(EntryType("mandatory")))).toSet
    AssignmentPlan(amount, entries)
  }
  val cgaPlan = {
    val amount = 6
    val entries = (0 until amount).map(n => AssignmentEntry(n, Set(EntryType("mandatory")))).toSet
    AssignmentPlan(amount, entries)
  }

  def populate = Action { request =>
    (rooms(5) ++
      roles ++
      authorities ++
      degrees ++
      semesters ++
      employees ++
      courses ++
      refroles ++
      labworks ++
      students ++ foo ++
      lApps).foldRight(Try(List[PointedGraph[repository.Rdf]]())) { (l, r) =>
      l match {
        case Success(g) => r map (_ :+ g)
        case Failure(e) => Failure(e)
      }
    } match {
      case Success(g) => Ok("Graph created")
      case Failure(e) => InternalServerError(e.getMessage)
    }
  }

  def getAdded = Action { request =>
    import bindings.RoleBinding
    import bindings.RoomBinding
    import bindings.EmployeeBinding
    import bindings.AuthorityBinding
    import bindings.DegreeBinding
    import bindings.SemesterBinding
    import bindings.CourseBinding
    import bindings.RefRoleBinding
    import bindings.StudentBinding
    import bindings.LabworkBinding
    import bindings.LabworkApplicationBinding

    (for {
      rooms <- repository.get[Room](RoomBinding.roomBinder, RoomBinding.classUri)
      roles <- repository.get[Role](RoleBinding.roleBinder, RoleBinding.classUri)
      auths <- repository.get[Authority](AuthorityBinding.authorityBinder, AuthorityBinding.classUri)
      degrees <- repository.get[Degree](DegreeBinding.degreeBinder, DegreeBinding.classUri)
      semesters <- repository.get[Semester](SemesterBinding.semesterBinder, SemesterBinding.classUri)
      people <- repository.get[Employee](EmployeeBinding.employeeBinder, EmployeeBinding.classUri)
      courses <- repository.get[Course](CourseBinding.courseBinder, CourseBinding.classUri)
      refrole <- repository.get[RefRole](RefRoleBinding.refRoleBinder, RefRoleBinding.classUri)
      labworks <- repository.get[Labwork](LabworkBinding.labworkBinder, LabworkBinding.classUri)
      students <- repository.get[Student](StudentBinding.studentBinder, StudentBinding.classUri)
      lApps <- repository.get[LabworkApplication](LabworkApplicationBinding.labworkApplicationBinder, LabworkApplicationBinding.classUri)
    } yield {
      List(
        Json.toJson(rooms),
        Json.toJson(roles),
        Json.toJson(auths),
        Json.toJson(degrees),
        Json.toJson(semesters),
        Json.toJson(people),
        Json.toJson(courses),
        Json.toJson(refrole),
        Json.toJson(labworks),
        Json.toJson(students),
        Json.toJson(lApps)
      )
    }) match {
      case Success(json) => Ok(json.foldLeft(JsArray())((l, r) => l ++ r.asInstanceOf[JsArray]))
      case Failure(e) => InternalServerError(e.getMessage)
    }
  }

  def rooms(n: Int) = {
    import bindings.RoomBinding._
    def roomgen(n: Int) = Stream.continually(Room(s"R ${nextInt(3)}.${nextInt(9)}${nextInt(9)}${nextInt(9)}", "Desc")).take(n) ++ List(Room("H32-LC", "H32-LC Desc"), Room("H32-BG", "H32-BG Desc"), Room("H32-HA", "H32-HA Desc"))
    roomgen(n) map repository.add[Room]
  }

  def roles = {
    import bindings.RoleBinding._
    List(adminRole, studentRole, employeeRole, mvRole, maRole, assistantRole, rvRole) map repository.add[Role]
}

  def people = List(Employee("ai1818", "Wurst", "Hans", "", Employee.randomUUID))

  def authorities = {
    import bindings.AuthorityBinding._
    import bindings.EmployeeBinding._

    people.map(p => (p, Authority(p.id, Set(adminRefRole.id)))).foldLeft(List[Try[PointedGraph[repository.Rdf]]]()) {
      case (l, (emp, auth)) => l :+ repository.add[Employee](emp) :+ repository.add[Authority](auth)
    }
  }

  def refroles = {
    import bindings.RefRoleBinding._
    List(
      adminRefRole,
      employeeRefRole,
      studentRefRole,
      rvRefRole
    ).map(repository.add[RefRole])
  }

  def degrees = {
    import bindings.DegreeBinding._

    List(
      Degree("Allgemeine Informatik", "AI", ai),
      Degree("Medieninformatik", "MI", mi),
      Degree("Technische Informatik", "TI", ti),
      Degree("Wirtschaftsinformatik", "WI", wi)
    ).map(repository.add[Degree])
  }

  def semesters = {
    import bindings.SemesterBinding._

    List(
      Semester("Sommersemester 2015", "SS 15", "2015-03-01", "2015-08-31", "2015-07-11", ss15),
      Semester("Wintersemester 2015/2016", "WS 15/16", "2015-09-01", "2016-02-29", "2016-02-01", ws1516),
      Semester("Sommersemester 2016", "SS 16", "2016-03-01", "2016-08-31", "2016-07-11", ss16)
    ).map(repository.add[Semester])
  }

  def courses = {
    import bindings.CourseBinding._

    List(
      Course("Mathematik 1", "Konen", "MA 1", konen, 1, ma1Konen),
      Course("Mathematik 1", "Leopold", "MA 1", leopold, 1, ma1Leopold),
      Course("Mathematik 2", "Schmitter", "MA 2", schmitter, 2, ma2Schmitter),
      Course("Algorithmen und Programmierung 1", "Victor", "AP 1", victor, 1, ap1Victor),
      Course("Computergrafik und Animation", "Eisemann", "CGA", eisemann, 3, cgaEisemann),
      Course("Algorithmen und Programmierung 2", "Kohls", "AP 2", kohls, 2, ap2Kohls)
    ).map(repository.add[Course])
  }

  def employees = {
    import bindings.EmployeeBinding._

    List(
      Employee("konen", "konen", "wolle", "wolle.konen@fh-koeln.de", konen),
      Employee("leopold", "leopold", "edda", "edda.leopold@fh-koeln.de", leopold),
      Employee("schmitter", "schmitter", "ane", "ane.schmitter@fh-koeln.de", schmitter),
      Employee("victor", "victor", "frank", "frank.victor@fh-koeln.de", victor),
      Employee("eisemann", "eisemann", "martin", "martin.eisemann@fh-koeln.de", eisemann),
      Employee("kohls", "kohls", "christian", "christian.kohls@fh-koeln.de", kohls)
    ).map(repository.add[Employee])
  }

  def labworks = {
    import bindings.LabworkBinding._

    List(
      Labwork("ap1 wi", "victor adv", ws1516, ap1Victor, wi, ap1Plan),
      Labwork("ap1 ai", "victor adv", ws1516, ap1Victor, ai, ap1Plan),
      Labwork("ap1 mi", "victor adv", ws1516, ap1Victor, mi, ap1Plan, ap1MiPrak),
      Labwork("ap1 ti", "victor adv", ws1516, ap1Victor, ti, ap1Plan),
      Labwork("ap2 wi", "kohls adv", ss15, ap2Kohls, wi, ap2Plan),
      Labwork("ap2 ai", "kohls adv", ss15, ap2Kohls, ai, ap2Plan),
      Labwork("ap2 mi", "kohls adv", ss15, ap2Kohls, mi, ap2Plan),
      Labwork("ap2 ti", "kohls adv", ss15, ap2Kohls, ti, ap2Plan),
      Labwork("ma1 wi", "leopold", ws1516, ma1Leopold, wi, ma1Plan),
      Labwork("ma1 ai", "konen breiderhoff", ws1516, ma1Konen, ai, ma1Plan),
      Labwork("ma1 mi", "konen breiderhoff", ws1516, ma1Konen, mi, ma1Plan),
      Labwork("ma1 ti", "konen breiderhoff", ws1516, ma1Konen, ti, ma1Plan),
      Labwork("ma2 ai", "schmitter breiderhoff", ss15, ma2Schmitter, ai, ma2Plan),
      Labwork("ma2 mi", "schmitter breiderhoff", ss15, ma2Schmitter, mi, ma2Plan),
      Labwork("ma2 ti", "schmitter breiderhoff", ss15, ma2Schmitter, ti, ma2Plan),
      Labwork("cga mi", "eisemann adv", ws1516, cgaEisemann, mi, cgaPlan)
    ).map(repository.add[Labwork])
  }

  def students = {
    import bindings.StudentBinding._

    List(
      Student("gmId dobrynin", "alex", "dobrynin", "dobrynin@gm.th-koeln.de", "111111", mi, alexStudent),
      Student("gmId muesse", "uwe", "muesse", "muesse@gm.th-koeln.de", "222222", mi, uweStudent),
      Student("gmId avram", "robert", "avram", "avram@gm.th-koeln.de", "333333", mi, robertStudent),
      Student("gmId hahn", "christian", "hahn", "hahn@gm.th-koeln.de", "444444", mi, christianStudent)
    ).map(repository.add[Student])
  }

  def lApps = {
    import bindings.LabworkApplicationBinding._

    List(
      LabworkApplication(ap1MiPrak, alexStudent, Set(robertStudent)),
      LabworkApplication(ap1MiPrak, robertStudent, Set(alexStudent)),
      LabworkApplication(ap1MiPrak, uweStudent, Set(christianStudent)),
      LabworkApplication(ap1MiPrak, christianStudent, Set(uweStudent))
    ).map(repository.add[LabworkApplication])
  }

  def foo = {
    import bindings.RefRoleBinding._

    List(
      RefRole(Some(ap2Kohls), mvRole.id)
    ).map(repository.add[RefRole])
  }
}