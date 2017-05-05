package models

/*
  missing permissions directly implicate either admin or god and are therefore not modelled
 */
object Permissions {

  lazy val all = room.all ++ degree.all ++ course.all ++
    labwork.all ++ labworkApplication.all ++ authority.all ++
    role.all ++ schedule.all ++ timetable.all ++ semester.all ++ group.all ++
    user.all ++ blacklist.all ++ entryType.all ++ reportCardEntry.all ++
    reportCardEntryType.all ++ reportCardEvaluation.all ++ assignmentPlan.all ++
    annotation.all ++ scheduleEntry.all
  val god = Permission("with great power comes great responsibility")
  val prime = Permission("Prime")

  object room {
    lazy val all = Set(getAll, get)
    val getAll = Permission("Room:getAll")
    val get = Permission("Room:get")
  }

  object degree {
    lazy val all = Set(getAll, get)
    val getAll = Permission("Degree:getAll")
    val get = Permission("Degree:get")
  }

  object course {
    lazy val all = Set(create, update, getAll, get)
    val create = Permission("Course:create")
    val update = Permission("Course:update")
    val getAll = Permission("Course:getAll")
    val get = Permission("Course:get")
  }

  object labwork {
    lazy val all = Set(create, update, getAll, get)
    val create = Permission("Labwork:create")
    val update = Permission("Labwork:update")
    val getAll = Permission("Labwork:getAll")
    val get = Permission("Labwork:get")
  }

  object labworkApplication {
    lazy val all = Set(create, update, delete, get, getAll)
    val create = Permission("LabworkApplication:create")
    val update = Permission("LabworkApplication:update")
    val delete = Permission("LabworkApplication:delete")
    val get = Permission("LabworkApplication:get")
    val getAll = Permission("LabworkApplication:getAll")
  }

  object authority {
    lazy val all = Set(create, delete, getAll, get)
    val create = Permission("Authority:create")
    val delete = Permission("Authority:delete")
    val getAll = Permission("Authority:getAll")
    val get = Permission("Authority:get")
  }

  object role {
    lazy val all = Set(getAll, get)
    val getAll = Permission("Role:getAll")
    val get = Permission("Role:get")
  }

  object schedule {
    lazy val all = Set(create, delete, get)
    val create = Permission("Schedule:create")
    val delete = Permission("Schedule:delete")
    val get = Permission("Schedule:get")
  }

  object scheduleEntry {
    lazy val all = Set(update, get, getAll)
    val update = Permission("ScheduleEntry:update")
    val get = Permission("ScheduleEntry:get")
    val getAll = Permission("ScheduleEntry:getAll")
  }

  object timetable {
    lazy val all = Set(create, update, delete, get, getAll)
    val create = Permission("Timetable:create")
    val update = Permission("Timetable:update")
    val delete = Permission("Timetable:delete")
    val get = Permission("Timetable:get")
    val getAll = Permission("Timetable:getAll")
  }

  object semester {
    lazy val all = Set(getAll, get)
    val get = Permission("Semester:get")
    val getAll = Permission("Semester:getAll")
  }

  object group {
    lazy val all = Set(create, getAll)
    val create = Permission("Group:create")
    val getAll = Permission("Group:getAll")
  }

  object user {
    lazy val all = Set(get, getAll)
    val get = Permission("User:get")
    val getAll = Permission("User:getAll")
  }

  object blacklist {
    lazy val all = Set(get, getAll)
    val get = Permission("Blacklist:get")
    val getAll = Permission("Blacklist:getAll")
  }

  object entryType {
    lazy val all = Set(getAll)
    val getAll = Permission("EntryType:getAll")
  }

  object reportCardEntry {
    lazy val all = Set(create, update, get, getAll)
    val create = Permission("ReportCardEntry:create")
    val update = Permission("ReportCardEntry:update")
    val get = Permission("ReportCardEntry:get")
    val getAll = Permission("ReportCardEntry:getAll")
  }

  object reportCardEntryType {
    lazy val all = Set(update)
    val update = Permission("ReportCardEntryType:update")
  }

  object reportCardEvaluation {
    lazy val all = Set(create, get, getAll)
    val create = Permission("ReportCardEvaluation:create")
    val get = Permission("ReportCardEvaluation:get")
    val getAll = Permission("ReportCardEvaluation:getAll")
  }

  object assignmentPlan {
    lazy val all = Set(create, update, delete, getAll, get)
    val create = Permission("AssignmentPlan:create")
    val update = Permission("AssignmentPlan:update")
    val delete = Permission("AssignmentPlan:delete")
    val getAll = Permission("AssignmentPlan:getAll")
    val get = Permission("AssignmentPlan:get")
  }

  object annotation {
    lazy val all = Set(create, update, delete, getAll, get)
    val create = Permission("Annotation:create")
    val update = Permission("Annotation:update")
    val delete = Permission("Annotation:delete")
    val getAll = Permission("Annotation:getAll")
    val get = Permission("Annotation:get")
  }
}