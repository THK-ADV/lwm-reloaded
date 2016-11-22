package modules

import controllers.EntryTypeController

trait EntryTypeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  def entryTypeController: EntryTypeController
}

trait DefaultEntryTypeManagementModule extends EntryTypeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  override lazy val entryTypeController: EntryTypeController = new EntryTypeController(repository, sessionService, namespace, roleService)
}
