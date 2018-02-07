package modules

import controllers.ScheduleEntryControllerPostgres
import dao.{ScheduleEntryDao, ScheduleEntryDaoImpl}

trait ScheduleEntryDaoModule {
  self: DatabaseModule =>

  def scheduleEntryDao: ScheduleEntryDao
}

trait DefaultScheduleEntryDaoModule extends ScheduleEntryDaoModule {
  self: DatabaseModule =>

  override lazy val scheduleEntryDao = new ScheduleEntryDaoImpl(db)
}

trait ScheduleEntryManagementModule2 {
  self: ScheduleEntryDaoModule with AuthorityDaoModule with SessionRepositoryModule with ScheduleServiceManagementModule
    with AssignmentPlanDaoModule with LabworkDaoModule with TimetableDaoManagementModule with LabworkApplicationDaoModule
    with GroupDaoManagementModule =>

  def scheduleEntryControllerPostgres: ScheduleEntryControllerPostgres
}

trait DefaultScheduleEntryManagementModule2 extends ScheduleEntryManagementModule2 {
  self: ScheduleEntryDaoModule with AuthorityDaoModule with SessionRepositoryModule with ScheduleServiceManagementModule
    with AssignmentPlanDaoModule with LabworkDaoModule with TimetableDaoManagementModule with LabworkApplicationDaoModule
    with GroupDaoManagementModule =>

  override lazy val scheduleEntryControllerPostgres = new ScheduleEntryControllerPostgres(authorityDao, sessionService, scheduleEntryDao, scheduleService,
    assignmentPlanDao, labworkDao, timetableDao, labworkApplicationDao, groupDao)
}