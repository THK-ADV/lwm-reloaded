package controllers

import java.util.UUID

import models._
import models.applications.LabworkApplication
import models.schedule.{Weekday, TimetableEntry, Timetable}
import models.security.{Authority, RefRole, Role, Roles}
import models.security.Roles._
import models.semester.{Blacklist, Semester}
import models.users.{User, Employee, Student}
import org.joda.time.format.DateTimeFormat
import org.joda.time.{LocalTime, LocalDate}
import org.w3.banana.PointedGraph
import play.api.libs.json.{JsArray, Json}
import play.api.mvc.{Action, Controller}
import store.SesameRepository
import store.bind.Bindings

import scala.language.implicitConversions
import scala.util.Random._
import scala.util.{Failure, Success, Try}

object ApiDataController {
  import models.security.Permissions._

  val mvRole = Role(CourseManager,
    labwork.all ++ schedule.all ++ timetable.all ++ group.all ++ reportCard.all ++ assignmentPlan.all + course.update
  )
  val maRole = Role(CourseEmployee,
    Set(labwork.get, labwork.getAll) ++ Set(schedule.get, schedule.getAll) ++ Set(timetable.get, timetable.getAll) ++ reportCard.all + group.get + assignmentPlan.get
  )
  val assistantRole = Role(CourseAssistant,
    Set(schedule.get, timetable.get) ++ reportCard.all
  )

  val rvRole = Role(RightsManager, authority.all ++ refRole.all ++ role.all)
  val studentRole = Role(Roles.Student,
    Set(room.get, degree.get, course.get, labwork.get) ++
      Set(labworkApplication.create, labworkApplication.update, labworkApplication.delete, labworkApplication.get) +
      semester.get + group.get + user.get + reportCard.get
  )
  val employeeRole = Role(Roles.Employee,
    room.all ++ semester.all ++ degree.all ++ user.all + blacklist.get ++ Set(course.get, course.getAll) + labworkApplication.getAll ++ entryType.all
  )
  val adminRole = Role(Admin, Set(prime))
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

  val konen = User.randomUUID
  val leopold = User.randomUUID
  val schmitter = User.randomUUID
  val victor = User.randomUUID
  val eisemann = User.randomUUID
  val kohls = User.randomUUID

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
  val ap1WiPrak = Labwork.randomUUID
  val ap1TiPrak = Labwork.randomUUID
  val ap1AiPrak = Labwork.randomUUID
  val ap2WiPrak = Labwork.randomUUID
  val ap2TiPrak = Labwork.randomUUID
  val ap2AiPrak = Labwork.randomUUID
  val ap2MiPrak = Labwork.randomUUID
  val ma1WiPrak = Labwork.randomUUID
  val ma1AiPrak = Labwork.randomUUID
  val ma1TiPrak = Labwork.randomUUID
  val ma1MiPrak = Labwork.randomUUID
  val ma2AiPrak = Labwork.randomUUID
  val ma2MiPrak = Labwork.randomUUID
  val ma2TiPrak = Labwork.randomUUID
  val cgaMiPrak = Labwork.randomUUID

  val alexStudent = User.randomUUID
  val uweStudent = User.randomUUID
  val robertStudent = User.randomUUID
  val christianStudent = User.randomUUID

  val randomStudents = {
    val degrees = Vector(ai, mi, ti, wi)
    (0 until 400).map(n =>
      Student(n.toString, n.toString, n.toString, n.toString, n.toString, degrees(nextInt(degrees.size)), User.randomUUID)
    ).toList
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
      students ++
      plans ++
      timetables ++
      blacklists ++
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

  def reportCard(user: String) = Action { request =>
    import bindings.ReportCardBinding._
    import models.AssignmentEntryType._

    val entries = (0 until 5).map { n =>
      val start = LocalTime.now.plusHours(n)
      ReportCardEntry(n, n.toString, LocalDate.now, start, start.plusHours(1), UUID.randomUUID(), Set(ReportCardEntryType.Attendance))
    }.toSet
    val card = ReportCard(UUID.fromString(user), ap1MiPrak, entries)

    repository.add[ReportCard](card) match {
      case Success(_) => Ok(Json.obj(
        "status" -> "Ok"
      ))
      case Failure(e) => InternalServerError(Json.obj(
        "error" -> e.getMessage
      ))
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

  def people = List(Employee("ai1818", "Wurst", "Hans", "", "employee", User.randomUUID))

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
      Employee("konen", "konen", "wolle", "wolle.konen@fh-koeln.de", "lecturer", konen),
      Employee("leopold", "leopold", "edda", "edda.leopold@fh-koeln.de", "lecturer", leopold),
      Employee("schmitter", "schmitter", "ane", "ane.schmitter@fh-koeln.de", "lecturer", schmitter),
      Employee("victor", "victor", "frank", "frank.victor@fh-koeln.de", "lecturer", victor),
      Employee("eisemann", "eisemann", "martin", "martin.eisemann@fh-koeln.de", "lecturer", eisemann),
      Employee("kohls", "kohls", "christian", "christian.kohls@fh-koeln.de", "lecturer", kohls)
    ).map(repository.add[Employee])
  }

  def labworks = {
    import bindings.LabworkBinding._

    List(
      Labwork("ap1 wi", "victor adv", ws1516, ap1Victor, wi, ap1WiPrak),
      Labwork("ap1 ai", "victor adv", ws1516, ap1Victor, ai, ap1AiPrak),
      Labwork("ap1 mi", "victor adv", ws1516, ap1Victor, mi, ap1MiPrak),
      Labwork("ap1 ti", "victor adv", ws1516, ap1Victor, ti, ap1TiPrak),
      Labwork("ap2 wi", "kohls adv", ss15, ap2Kohls, wi, ap2WiPrak),
      Labwork("ap2 ai", "kohls adv", ss15, ap2Kohls, ai, ap2AiPrak),
      Labwork("ap2 mi", "kohls adv", ss15, ap2Kohls, mi, ap2MiPrak),
      Labwork("ap2 ti", "kohls adv", ss15, ap2Kohls, ti, ap2TiPrak),
      Labwork("ma1 wi", "leopold", ws1516, ma1Leopold, wi, ma1WiPrak),
      Labwork("ma1 ai", "konen breiderhoff", ws1516, ma1Konen, ai, ma1AiPrak),
      Labwork("ma1 mi", "konen breiderhoff", ws1516, ma1Konen, mi, ma1MiPrak),
      Labwork("ma1 ti", "konen breiderhoff", ws1516, ma1Konen, ti, ma1TiPrak),
      Labwork("ma2 ai", "schmitter breiderhoff", ss15, ma2Schmitter, ai, ma2AiPrak),
      Labwork("ma2 mi", "schmitter breiderhoff", ss15, ma2Schmitter, mi, ma2MiPrak),
      Labwork("ma2 ti", "schmitter breiderhoff", ss15, ma2Schmitter, ti, ma2TiPrak),
      Labwork("cga mi", "eisemann adv", ws1516, cgaEisemann, mi, cgaMiPrak)
    ).map(repository.add[Labwork])
  }

  def students = {
    import bindings.StudentBinding._
    import scala.util.Random._

    (List(
      Student("gmId dobrynin", "alex", "dobrynin", "dobrynin@gm.th-koeln.de", "111111", mi, alexStudent),
      Student("gmId muesse", "uwe", "muesse", "muesse@gm.th-koeln.de", "222222", mi, uweStudent),
      Student("gmId avram", "robert", "avram", "avram@gm.th-koeln.de", "333333", mi, robertStudent),
      Student("gmId hahn", "christian", "hahn", "hahn@gm.th-koeln.de", "444444", mi, christianStudent)
    ) ++ randomStudents).map(repository.add[Student])
  }

  def lApps = {
    import bindings.LabworkApplicationBinding._
    import scala.util.Random._

    val ap1Praks = Vector(ap1MiPrak, ap1AiPrak, ap1TiPrak, ap1WiPrak)
    val ma1Praks = Vector(ma1MiPrak, ma1AiPrak, ma1TiPrak, ma1WiPrak)
    val ap1Apps = randomStudents.map(s => LabworkApplication(ap1Praks(nextInt(ap1Praks.size)), s.id, Set.empty))
    val ma1Apps = randomStudents.map(s => LabworkApplication(ma1Praks(nextInt(ma1Praks.size)), s.id, Set.empty))

    (List(
      LabworkApplication(ap1MiPrak, alexStudent, Set(robertStudent)),
      LabworkApplication(ap1MiPrak, robertStudent, Set(alexStudent)),
      LabworkApplication(ap1MiPrak, uweStudent, Set(christianStudent)),
      LabworkApplication(ap1MiPrak, christianStudent, Set(uweStudent))
    ) ++ ap1Apps ++ ma1Apps).map(repository.add[LabworkApplication])
  }

  def plans = {
    import models.AssignmentEntryType._
    import bindings.AssignmentPlanBinding._

    def ap1Plan(labwork: UUID): AssignmentPlan = {
      val amount = 8
      val entries = Set(
        AssignmentEntry(0, "Einführung", Set(Attendance)),
        AssignmentEntry(1, "Liveaufgabe 1 - C", Set(Attendance, Certificate)),
        AssignmentEntry(2, "Liveaufgabe 2 - C", Set(Attendance, Certificate)),
        AssignmentEntry(3, "Ilias Test", Set(Attendance, Certificate, Bonus)),
        AssignmentEntry(4, "Liveaufgabe 3 - Java", Set(Attendance, Certificate)),
        AssignmentEntry(5, "Liveaufgabe 4 - Java", Set(Attendance, Certificate)),
        AssignmentEntry(6, "Codereview", Set(Attendance, Certificate, Supplement)),
        AssignmentEntry(7, "Codereview", Set(Attendance, Certificate, Supplement))
      )
      AssignmentPlan(labwork, amount, amount - 1, entries)
    }
    def genPlan(labwork: UUID, amount: Int): AssignmentPlan = {
      import scala.util.Random._

      val types = all.toVector
      val random = shuffle(types).take(nextInt(types.size)).toSet

      val entries = (0 until amount).map(n => AssignmentEntry(n, "foo", random)).toSet
      AssignmentPlan(labwork, amount, amount, entries)
    }

    (List(ap1WiPrak, ap1MiPrak, ap1TiPrak, ap1AiPrak).map(ap1Plan) ++
    List(ap2WiPrak, ap2MiPrak, ap2TiPrak, ap2AiPrak).map(genPlan(_, 6)) ++
    List(ma1WiPrak, ma1MiPrak, ma1TiPrak, ma1AiPrak).map(genPlan(_, 4)) ++
    List(ma2MiPrak, ma2TiPrak, ma2AiPrak).map(genPlan(_, 4)) ++
    List(cgaMiPrak).map(genPlan(_, 6))).map(repository.add[AssignmentPlan])
  }

  def timetables = {
    import bindings.TimetableBinding._

    val ft = DateTimeFormat.forPattern("HH:mm:ss")
    val fd = DateTimeFormat.forPattern("dd/MM/yyyy")

    val ap1MiEntries = Set(
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("15:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("15:00:00"), ft.parseLocalTime("16:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("16:00:00"), ft.parseLocalTime("17:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("09:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("09:00:00"), ft.parseLocalTime("10:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("10:00:00"), ft.parseLocalTime("11:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("12:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("12:00:00"), ft.parseLocalTime("13:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ap1MiPrak, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("13:00:00"), ft.parseLocalTime("14:00:00"))
    )
    val ma1MiEntries = Set(
      TimetableEntry(User.randomUUID, Room.randomUUID, ma1MiPrak, Weekday.toDay(fd.parseLocalDate("26/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ma1MiPrak, Weekday.toDay(fd.parseLocalDate("27/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ma1MiPrak, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("08:00:00"), ft.parseLocalTime("11:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ma1MiPrak, Weekday.toDay(fd.parseLocalDate("29/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ma1MiPrak, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("11:00:00"), ft.parseLocalTime("14:00:00")),
      TimetableEntry(User.randomUUID, Room.randomUUID, ma1MiPrak, Weekday.toDay(fd.parseLocalDate("30/10/2015")).index, ft.parseLocalTime("14:00:00"), ft.parseLocalTime("17:00:00"))
    )

    List(
      Timetable(ap1MiPrak, ap1MiEntries, fd.parseLocalDate("27/10/2015"), Blacklist.empty, Timetable.randomUUID),
      Timetable(ma1MiPrak, ma1MiEntries, fd.parseLocalDate("26/10/2015"), Blacklist.empty, Timetable.randomUUID)
    ).map(repository.add[Timetable])
  }

  def blacklists = {
    import bindings.BlacklistBinding._

    val fd = DateTimeFormat.forPattern("dd/MM/yyyy")

    val profileWeek = (0 until 5).map(n => fd.parseDateTime("23/11/2015").plusDays(n)).toSet
    val christmas = (0 until 3 * 7).map(n => fd.parseDateTime("21/12/2015").plusDays(n)).toSet

    List(
      Blacklist(profileWeek, Blacklist.randomUUID),
      Blacklist(christmas, Blacklist.randomUUID)
    ).map(repository.add[Blacklist])
  }

  def foo = {
    import bindings.RefRoleBinding._

    List(
      RefRole(Some(ap2Kohls), mvRole.id)
    ).map(repository.add[RefRole])
  }
}