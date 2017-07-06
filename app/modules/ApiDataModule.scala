package modules

import controllers.ApiDataController

trait ApiDataModule {
  self: SemanticRepositoryModule =>

  def apiDataController: ApiDataController
}

trait DefaultApiDataModule extends ApiDataModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  override lazy val apiDataController: ApiDataController = new ApiDataController(repository, sessionService, roleService)
}
