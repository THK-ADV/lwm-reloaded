package store

import org.w3.banana.{RDF, RDFOps}

object Prefixes {

  class LWMPrefix[Rdf <: RDF](ops: RDFOps[Rdf]) extends PrefixBuilder("lwm", "http://lwm.fh-koeln.de/ns/")(ops) {

    // xsd extensions
    val localDate = apply("localDate")
    val localTime = apply("localTime")

    // _
    val id = apply("id")
    val invalidated = apply("invalidated")

    //Labwork, Course, Degree, Group, Room, Semester, AssignmentEntry, ReportCardEntry, Role, ReportCardEvaluation
    val label = apply("label")

    // Labwork, LabworkApplication, Room, Degree, TimetableEntry, RefRole, Annotation, ReportCardEvaluation
    val description = apply("description")
    val assignmentPlan = apply("assignmentPlan")
    val semester = apply("semester")
    val course = apply("course")
    val degree = apply("degree")
    val applicant = apply("applicant")
    val timestamp = apply("timestamp")
    val friends = apply("friends")

    // Student, Employee, User
    val email = apply("email")
    val firstname = apply("firstname")
    val lastname = apply("lastname")
    val registrationId = apply("registrationId")
    val systemId = apply("systemId")
    val enrollment = apply("enrollment")
    val status = apply("status")

    // Semester, ReportCardEntry
    val end = apply("start")
    val start = apply("end")
    val examStart = apply("examStart")

    // AssignmentEntry, AssignmentPlan, Timetable, Schedule
    val index = apply("index")
    val duration = apply("duration")
    val entryTypes = apply("types")
    val entries = apply("entries")
    val attendance = apply("attendance")
    val mandatory = apply("mandatory")

    // AssignmentPlanEntry
    val entryType = apply("entryType")
    val bool = apply("bool")
    val int = apply("int")

    // Blacklist
    val dates = apply("dates")

    // Course
    val abbreviation = apply("abbreviation")
    val lecturer = apply("lecturer")
    val semesterIndex = apply("semesterIndex")

    // RefRole, Role, Authority
    val role = apply("role")
    val permissions = apply("permissions")
    val privileged = apply("privileged")

    // Group, Timetable, Schedule, Annotation, Assignmentplan, ReportCardEntry, ReportCardEvaluation
    val members = apply("members")
    val labwork = apply("labwork")

    // Timetable
    val blacklist = apply("blacklist")
    val buffer = apply("buffer")

    // TimetableEntry, ScheduleEntry, ReportCardEntry
    val supervisor = apply("supervisor")
    val room = apply("room")
    val dayIndex = apply("dayIndex")
    val date = apply("date")

    // ReportCardEntry
    val rescheduled = apply("rescheduled")

    // ScheduleEntry
    val group = apply("group")

    // Annotation, ReportCardEntry, ReportCardEvaluation
    val student = apply("student")

    // Labwork
    val subscribable = apply("subscribable")
    val published = apply("published")

    // Annotation
    val reportCardEntry = apply("reportCardEntry")
    val message = apply("message")

    // classes
    val User = apply("User")
    val Course = apply("Course")
    val Degree = apply("Degree")
    val Group = apply("Group")
    val Labwork = apply("Labwork")
    val Room = apply("Room")
    val Semester = apply("Semester")
    val Role = apply("Role")
    val RefRole = apply("RefRole")
    val Authority = apply("Authority")
    val AssignmentPlan = apply("AssignmentPlan")
    val AssignmentEntry = apply("AssignmentEntry")
    val AssignmentEntryType = apply("AssignmentEntryType")
    val LabworkApplication = apply("LabworkApplication")
    val Timetable = apply("Timetable")
    val TimetableEntry = apply("TimetableEntry")
    val Schedule = apply("Schedule")
    val ScheduleEntry = apply("ScheduleEntry")
    val Blacklist = apply("Blacklist")
    val ReportCardEntry = apply("ReportCardEntry")
    val ReportCardEntryType = apply("ReportCardEntryType")
    val Rescheduled = apply("Rescheduled")
    val Annotation = apply("Annotation")
    val ReportCardEvaluation = apply("ReportCardEvaluation")
  }

  object LWMPrefix {
    def apply[Rdf <: RDF : RDFOps](implicit ops: RDFOps[Rdf]) = new LWMPrefix(ops)
  }

}
