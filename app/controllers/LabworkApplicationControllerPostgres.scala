package controllers

import java.util.UUID

import models.{LabworkApplication, LabworkApplicationDb, SesameLabworkApplicationProtocol}
import services.{LabworkApplicationService2, RoleServiceLike, SessionHandlingService}
import store.{LabworkApplicationTable, TableFilter}

import scala.util.Try

final class LabworkApplicationControllerPostgres(val sessionService: SessionHandlingService, val roleService: RoleServiceLike, val labworkApplicationService: LabworkApplicationService2)
  extends AbstractCRUDControllerPostgres[SesameLabworkApplicationProtocol, LabworkApplicationTable, LabworkApplicationDb, LabworkApplication] {

  override protected implicit def writes = ???

  override protected implicit def reads = ???

  override protected def abstractDao = ???

  override protected def idTableFilter(id: String) = ???

  override protected def tableFilter(attribute: String, values: Seq[String])(appendTo: Try[List[TableFilter[LabworkApplicationTable]]]) = ???

  override protected def toDbModel(protocol: SesameLabworkApplicationProtocol, existingId: Option[UUID]) = ???

  override protected def toLwmModel(dbModel: LabworkApplicationDb) = ???

  override implicit def mimeType = ???
}
