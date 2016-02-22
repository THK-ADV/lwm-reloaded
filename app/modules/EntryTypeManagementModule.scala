package modules

import controllers.EntryTypeController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait EntryTypeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  def entryTypeController: EntryTypeController
}

trait DefaultEntryTypeManagementModule extends EntryTypeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  override def entryTypeController: EntryTypeController = new EntryTypeController(repository, namespace, roleService)
}
