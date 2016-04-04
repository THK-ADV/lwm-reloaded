package modules

import controllers.ApiDataController
import modules.store.SemanticRepositoryModule

trait ApiDataModule { self: SemanticRepositoryModule with LDAPModule =>
  def apiDataController: ApiDataController
}

trait DefaultApiDataModule extends ApiDataModule{ self: SemanticRepositoryModule with LDAPModuleImpl =>
  override def apiDataController: ApiDataController = new ApiDataController(repository, ldap)
}
