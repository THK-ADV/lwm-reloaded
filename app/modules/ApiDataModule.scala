package modules

import controllers.ApiDataController
import modules.store.SemanticRepositoryModule

trait ApiDataModule { self: SemanticRepositoryModule =>
  def apiDataController: ApiDataController
}

trait DefaultApiDataModule extends ApiDataModule{ self: SemanticRepositoryModule =>
  override def apiDataController: ApiDataController = new ApiDataController(repository)
}
