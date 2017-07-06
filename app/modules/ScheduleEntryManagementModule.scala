package modules

import controllers.{ScheduleEntryController, ScheduleEntryControllerPostgres}
import services.{ScheduleEntryDao, ScheduleEntryDaoImpl}
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

trait ScheduleEntryManagementModule2 { self: LwmApplication
  with ScheduleEntryDaoModule with SecurityManagementModule with SessionRepositoryModule with ScheduleServiceManagementModule
  with AssignmentPlanServiceModule with LabworkServiceModule with TimetableService2ManagementModule with LabworkApplication2ServiceModule =>

  def scheduleEntryControllerPostgres: ScheduleEntryControllerPostgres
}

trait DefaultScheduleEntryManagementModule2 extends ScheduleEntryManagementModule2 { self: LwmApplication
  with ScheduleEntryDaoModule with SecurityManagementModule with SessionRepositoryModule with ScheduleServiceManagementModule
  with AssignmentPlanServiceModule with LabworkServiceModule with TimetableService2ManagementModule with LabworkApplication2ServiceModule =>

  override lazy val scheduleEntryControllerPostgres = new ScheduleEntryControllerPostgres(roleService, sessionService, scheduleEntryDao, scheduleService,
    assignmentPlanService, labworkService, timetableService2, labworkApplicationService2)
}