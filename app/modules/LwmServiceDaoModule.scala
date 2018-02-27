package modules

import controllers.LwmServiceController
import dao.{LwmServiceDao, LwmServiceDaoImpl}

trait LwmServiceDaoModule {
  self: DatabaseModule with LabworkApplicationDaoModule with GroupDaoManagementModule with ReportCardEntryDaoModule =>

  def lwmServiceDao: LwmServiceDao
}

trait DefaultLwmServiceDaoModule {
  self: DatabaseModule with LabworkApplicationDaoModule with GroupDaoManagementModule with ReportCardEntryDaoModule =>

  lazy val lwmServiceDao: LwmServiceDao = new LwmServiceDaoImpl(db, labworkApplicationDao, groupDao, reportCardEntryDao)
}

trait LwmServiceControllerModule {
  self: AuthorityDaoModule with SessionRepositoryModule with LwmServiceDaoModule =>

  def lwmServiceController: LwmServiceController
}

trait DefaultLwmServiceControllerModule {
  self: AuthorityDaoModule with SessionRepositoryModule with LwmServiceDaoModule =>

  lazy val lwmServiceController: LwmServiceController = new LwmServiceController(authorityDao, sessionService, lwmServiceDao)
}