package modules

import controllers.EntryTypeController

trait EntryTypeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  def entryTypeController: EntryTypeController
}

trait DefaultEntryTypeManagementModule extends EntryTypeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  override def entryTypeController: EntryTypeController = new EntryTypeController(repository, namespace, roleService)
}
