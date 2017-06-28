package modules

import controllers.ReportCardEntryController
import services.{ReportCardEntryDao, ReportCardEntryDaoImpl}

trait ReportCardEntryManagementModule {

  def reportCardEntryManagementController: ReportCardEntryController
}

trait DefaultReportCardEntryManagementModuleImpl extends ReportCardEntryManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule with ReportCardServiceManagementModule =>

  lazy val reportCardEntryManagementController: ReportCardEntryController = new ReportCardEntryController(repository, sessionService, namespace, roleService, reportCardService)
}

// POSTGRES

trait ReportCardEntryDaoModule {
  self: DatabaseModule =>

  def reportCardEntryDao: ReportCardEntryDao
}

trait DefaultReportCardEntryDaoModule extends ReportCardEntryDaoModule {
  self: DatabaseModule =>

  override lazy val reportCardEntryDao = new ReportCardEntryDaoImpl(db)
}