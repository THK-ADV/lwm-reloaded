package modules

import controllers.EntryTypeController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait EntryTypeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>
  def entryTypeController: EntryTypeController
}

trait DefaultEntryTypeManagementModule extends EntryTypeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>
  override def entryTypeController: EntryTypeController = new EntryTypeController(repository, sessionService, namespace, roleService)
}
