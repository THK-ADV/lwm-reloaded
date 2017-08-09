package modules

import controllers.{ReportCardEntryTypeController, ReportCardEntryTypeControllerPostgres}
import dao.{ReportCardEntryTypeDao, ReportCardEntryTypeDaoImpl}

trait ReportCardEntryTypeManagementModule {

  def reportCardEntryTypeManagementController: ReportCardEntryTypeController
}

trait DefaultReportCardEntryTypeManagementModuleImpl extends ReportCardEntryTypeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val reportCardEntryTypeManagementController: ReportCardEntryTypeController = new ReportCardEntryTypeController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait ReportCardEntryTypeDaoModule { self: DatabaseModule =>
  def reportCardEntryTypeDao: ReportCardEntryTypeDao
}

trait DefaultReportCardEntryTypeDaoModule extends ReportCardEntryTypeDaoModule { self: DatabaseModule =>
  override lazy val reportCardEntryTypeDao = new ReportCardEntryTypeDaoImpl(db)
}

trait ReportCardEntryTypeManagementModule2 {
  self: AuthorityDaoModule with SessionRepositoryModule with ReportCardEntryTypeDaoModule =>

  def reportCardEntryTypeController: ReportCardEntryTypeControllerPostgres
}

trait DefaultReportCardEntryTypeManagementModule2 extends ReportCardEntryTypeManagementModule2 {
  self: AuthorityDaoModule with SessionRepositoryModule with ReportCardEntryTypeDaoModule =>

  override lazy val reportCardEntryTypeController = new ReportCardEntryTypeControllerPostgres(authorityDao, sessionService, reportCardEntryTypeDao)
}