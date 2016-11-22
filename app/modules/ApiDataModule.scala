package modules

import controllers.ApiDataController

trait ApiDataModule {
  self: SemanticRepositoryModule =>

  def apiDataController: ApiDataController
}

trait DefaultApiDataModule extends ApiDataModule {
  self: SemanticRepositoryModule =>

  override lazy val apiDataController: ApiDataController = new ApiDataController(repository)
}
