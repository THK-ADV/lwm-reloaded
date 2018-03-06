package modules

import controllers.ApiDataController

trait ApiDataModule {
  self: SemanticRepositoryModule =>

  def apiDataController: ApiDataController
}

trait DefaultApiDataModule extends ApiDataModule {
  self: SemanticRepositoryModule with UserDaoModule with AssignmentPlanDaoModule with CourseDaoModule with DegreeDaoModule with LabworkApplicationDaoModule
    with LabworkDaoModule with RoleDaoModule with RoomDaoModule with SemesterDaoModule with TimetableDaoManagementModule with BlacklistDaoManagementModule
    with ReportCardEntryDaoModule with AuthorityDaoModule with ScheduleEntryDaoModule with GroupDaoManagementModule with SessionRepositoryModule
    with ReportCardEvaluationDaoModule with ReportCardEvaluationPatternDaoModule =>

  override lazy val apiDataController: ApiDataController = new ApiDataController(repository, userDao, assignmentPlanDao, courseDao, degreeDao, labworkApplicationDao,
    labworkDao, roleDao, roomDao, semesterDao, timetableDao, blacklistDao, reportCardEntryDao, authorityDao, scheduleEntryDao, groupDao, reportCardEvaluationDao,
    reportCardEvaluationPatternDao, sessionService)
}
