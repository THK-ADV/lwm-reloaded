package store

import org.w3.banana.{RDF, RDFOps}

object Prefixes {

  class LWMPrefix[Rdf <: RDF](ops: RDFOps[Rdf]) extends PrefixBuilder("lwm", "http://lwm.fh-koeln.de/ns/")(ops) {


    //AssignmentAssociation, Solution
    val assignment = apply("assignment")
    //SupervisionInformation
    val attended = apply("attended")
    //SupervisionInformation
    val comment = apply("comment")
    //GroupScheduleAssociation, StudentScheduleAssociation
    val date = apply("date")
    //Assignment, Error
    val description = apply("description")
    //AssignmentAssociation
    val dueIndex = apply("dueIndex")
    //Student, User
    val email = apply("email")
    //Semester
    val endDate = apply("endDate")
    //TimetableEntry
    val endTime = apply("endTime")
    //Student, User
    val firstname = apply("firstname")
    //Assignment
    val goals = apply("goals")
    //Group
    val groupSchedule = apply("groupSchedule")
    //StudentScheduleAssociation
    val groupScheduleAssociation = apply("groupScheduleAssociation")
    //Assignment
    val hints = apply("hints")
    //Student, User, Labwork, AssignmentAssociation, Assignment, Course, Degree, Error, Group, GroupSchedule,
    //GroupScheduleAssociation, Room, Solution, StudentSchedule, StudentScheduleAssociation, SupervisionInformation,
    //Tag, Timetable, TimetableEntry
    val id = apply("id")
    //AssignmentAssociation
    val index = apply("index")
    //Labwork, Course, Degree, Group, Room
    val label = apply("label")
    //AssignmentAssociation, Group
    val labwork = apply("labwork")
    //Student, User
    val lastname = apply("lastname")
    //Course
    val lecturer = apply("lecturer")
    //Semester, Tag
    val name = apply("name")
    //SupervisionInformation
    val passed = apply("passed")
    //Student
    val registrationId = apply("registrationId")
    //TimetableEntry
    val room = apply("room")
    //Assignment
    val solution = apply("solution")
    //Semester
    val startDate = apply("startDate")
    //TimetableEntry
    val startTime = apply("startTime")
    //TimetableEntry
    val supervisor = apply("supervisor")
    val systemId = apply("systemId")
    //Assignment
    val text = apply("text")
    //GroupScheduleAssociation, StudentScheduleAssociation
    val timetableEntry = apply("timetableEntry")
    //Assignment
    val title = apply("title")
    //AssignmentAssociation
    val visible = apply("visible")
    //Semester
    val examPeriod = apply("examPeriod")
    //Roles-Permissions
    val refroles = apply("refRoles")
    val role = apply("role")
    val module = apply("module")
    val privileged = apply("privileged")

    val Assignment = apply("Assignment")
    val AssignmentAssociation = apply("AssignmentAssociation")
    val Course = apply("Course")
    val Degree = apply("Degree")
    val Employee = apply("Employee")
    val Group = apply("Group")
    val GroupSchedule = apply("GroupSchedule")
    val GroupScheduleAssociation = apply("GroupScheduleAssociation")
    val Labwork = apply("Labwork")
    val Room = apply("Room")
    val Semester = apply("Semester")
    val Solution = apply("Solution")
    val Student = apply("Student")
    val StudentSchedule = apply("StudentSchedule")
    val StudentScheduleAssociation = apply("StudentScheduleAssociation")
    val SupervisionInformation = apply("SupervisionInformation")
    val Tag = apply("Tag")
    val Timetable = apply("Timetable")
    val TimetableEntry = apply("TimetableEntry")
    val User = apply("User")
    val RefRole = apply("RefRole")
    val Authority = apply("Authority")

  }

  object LWMPrefix {
    def apply[Rdf <: RDF : RDFOps](implicit ops: RDFOps[Rdf]) = new LWMPrefix(ops)
  }

}
