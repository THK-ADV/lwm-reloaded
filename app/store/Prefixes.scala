package store

import org.w3.banana.{RDF, RDFOps}

object Prefixes {

  class LWMPrefix[Rdf <: RDF](ops: RDFOps[Rdf]) extends PrefixBuilder("lwm", "http://lwm.fh-koeln.de/ns/")(ops) {

    // TODO merge this one out of here
    val localDate = apply("localDate")

    // _
    val id = apply("id")

    //Labwork, Course, Degree, Group, Room, Semster
    val label = apply("label")

    // Labwork, LabworkApplication, Room, Degree
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

    // Semester
    val endDate = apply("endDate")
    val startDate = apply("startDate")
    val examStart = apply("examStart")

    // AssignmentEntry, AssignmentPlan
    val index = apply("index")
    val types = apply("types")
    val entries = apply("entries")
    val numberOfEntries = apply("numberOfEntries")

    // Course
    val abbreviation = apply("abbreviation")
    val lecturer = apply("lecturer")

    // RefRole, Role, Authority
    val name = apply("name")
    val role = apply("role")
    val refroles = apply("refRoles")
    val permissions = apply("permissions")
    val module = apply("module")
    val privileged = apply("privileged")

    //Group
    val members = apply("members")
    val labwork = apply("labwork")

    // classes
    val Course = apply("Course")
    val Degree = apply("Degree")
    val Employee = apply("Employee")
    val Group = apply("Group")
    val Labwork = apply("Labwork")
    val Room = apply("Room")
    val Semester = apply("Semester")
    val Student = apply("Student")
    val Role = apply("Role")
    val RefRole = apply("RefRole")
    val Authority = apply("Authority")
    val AssignmentPlan = apply("AssignmentPlan")
    val AssignmentEntry = apply("AssignmentEntry")
    val LabworkApplication = apply("LabworkApplication")
  }

  object LWMPrefix {
    def apply[Rdf <: RDF : RDFOps](implicit ops: RDFOps[Rdf]) = new LWMPrefix(ops)
  }

}
