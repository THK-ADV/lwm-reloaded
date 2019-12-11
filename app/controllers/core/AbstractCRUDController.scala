package controllers.core

import controllers.helper._
import dao.helper.TableFilter
import database._
import javax.inject.Inject
import models.{UniqueDbEntity, UniqueEntity}
import play.api.mvc._
import slick.jdbc.PostgresProfile.api.Table

abstract class AbstractCRUDController[Protocol, T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity] @Inject()(
  cc: ControllerComponents
) extends AbstractController(cc)
  with Core[Protocol, T, DbModel, LwmModel]
  with Read[Protocol, T, DbModel, LwmModel]
  with Create[Protocol, T, DbModel, LwmModel]
  with Delete[Protocol, T, DbModel, LwmModel]
  with Update[Protocol, T, DbModel, LwmModel]
  with JsonBodyParser[Protocol, T, DbModel, LwmModel]
  with SecureControllerContext
  with ResultOps
  with JsonReads[Protocol]
  with JsonWrites[LwmModel]
  with Secured
  with AttributeFilter
  with RequestOps
  with TableFilter[T]