package modules

import controllers.ApiDataController
import modules.labwork.GroupServiceManagementModule
import modules.labwork.reportCard.ReportCardServiceManagementModule
import modules.labwork.schedule.ScheduleServiceManagementModule
import modules.store.SemanticRepositoryModule

trait ApiDataModule { self: SemanticRepositoryModule with LdapModule =>
  def apiDataController: ApiDataController
}

trait DefaultApiDataModule extends ApiDataModule {
  self: SemanticRepositoryModule with LdapModuleImpl with GroupServiceManagementModule with ScheduleServiceManagementModule with ReportCardServiceManagementModule  =>

  override def apiDataController: ApiDataController = new ApiDataController(repository, ldapService, groupService, scheduleService, reportCardService)
}
