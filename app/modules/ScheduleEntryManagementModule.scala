package modules

import controllers.{ScheduleEntryController, ScheduleEntryControllerPostgres}
import dao.{ScheduleEntryDao, ScheduleEntryDaoImpl}
import utils.LwmApplication

trait ScheduleEntryManagementModule { self: LwmApplication =>
  def scheduleEntryController: ScheduleEntryController
}

trait DefaultScheduleEntryManagementModule extends ScheduleEntryManagementModule { self: LwmApplication
  with SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val scheduleEntryController: ScheduleEntryController = new ScheduleEntryController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait ScheduleEntryDaoModule { self: DatabaseModule =>
  def scheduleEntryDao: ScheduleEntryDao
}

trait DefaultScheduleEntryDaoModule extends ScheduleEntryDaoModule { self: DatabaseModule =>
  override lazy val scheduleEntryDao = new ScheduleEntryDaoImpl(db)
}

trait ScheduleEntryManagementModule2 { self:
  ScheduleEntryDaoModule with AuthorityDaoModule with SessionRepositoryModule with ScheduleServiceManagementModule
  with AssignmentPlanDaoModule with LabworkDaoModule with TimetableDaoManagementModule with LabworkApplicationDaoModule
  with GroupDaoManagementModule =>

  def scheduleEntryControllerPostgres: ScheduleEntryControllerPostgres
}

trait DefaultScheduleEntryManagementModule2 extends ScheduleEntryManagementModule2 { self:
  ScheduleEntryDaoModule with AuthorityDaoModule with SessionRepositoryModule with ScheduleServiceManagementModule
  with AssignmentPlanDaoModule with LabworkDaoModule with TimetableDaoManagementModule with LabworkApplicationDaoModule
  with GroupDaoManagementModule =>

  override lazy val scheduleEntryControllerPostgres = new ScheduleEntryControllerPostgres(authorityDao, sessionService, scheduleEntryDao, scheduleService,
    assignmentPlanDao, labworkDao, timetableDao, labworkApplicationDao, groupDao)
}