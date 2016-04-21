package modules.labwork.schedule

import controllers.schedule.ScheduleEntryController
import modules.SessionRepositoryModule
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}
import utils.LwmApplication

trait ScheduleEntryManagementModule { self: LwmApplication =>
  def scheduleEntryController: ScheduleEntryController
}

trait DefaultScheduleEntryManagementModule extends ScheduleEntryManagementModule { self: LwmApplication
  with SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val scheduleEntryController: ScheduleEntryController = new ScheduleEntryController(repository, sessionService, namespace, roleService)
}