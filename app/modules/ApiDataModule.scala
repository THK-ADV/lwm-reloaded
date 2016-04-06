package modules

import controllers.ApiDataController
import modules.labwork.GroupServiceManagementModule
import modules.labwork.reportCard.ReportCardServiceManagementModule
import modules.labwork.schedule.ScheduleServiceManagementModule
import modules.store.SemanticRepositoryModule

trait ApiDataModule { self: SemanticRepositoryModule with LDAPModule =>
  def apiDataController: ApiDataController
}

trait DefaultApiDataModule extends ApiDataModule {
  self: SemanticRepositoryModule with LDAPModuleImpl with GroupServiceManagementModule with ScheduleServiceManagementModule with ReportCardServiceManagementModule  =>

  override def apiDataController: ApiDataController = new ApiDataController(repository, ldap, groupService, scheduleService, reportCardService)
}
