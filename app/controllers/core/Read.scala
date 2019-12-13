package controllers.core

import controllers.helper.{AttributeFilter, ResultOps, SecureControllerContext}
import dao.helper.TableFilter
import database.UniqueTable
import models.{UniqueDbEntity, UniqueEntity}
import play.api.mvc.{Action, AnyContent}
import slick.jdbc.PostgresProfile.api.Table

import scala.concurrent.Future
import scala.util.{Failure, Try}

trait Read[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity] {
  self: Core[_, T, DbModel, LwmModel]
    with SecureControllerContext
    with ResultOps
    with JsonWrites[LwmModel]
    with AttributeFilter
    with TableFilter[T] =>

  import DBFilterOps._

  protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] =
    Failure(new Throwable(s"no filter for $attribute and $value"))

  def all(secureContext: SecureContext = contextFrom(GetAll)): Action[AnyContent] = secureContext asyncAction { request =>
    import utils.Ops.MonadInstances.tryM
    import utils.Ops._

    def filter(queryString: QueryString): List[Try[TableFilterPredicate]] = {
      (for {
        (attribute, values) <- queryString if values.nonEmpty
      } yield makeTableFilter(attribute, values.head)).toList
    }

    val (queryString, defaults) = extractAttributes(request.queryString)

    (for {
      f <- Future.fromTry(filter(queryString).sequence)
      results <- abstractDao.get(f, defaults.atomic, defaults.valid, defaults.lastModified)
    } yield results).jsonResult
  }

  def get(id: String, secureContext: SecureContext = contextFrom(Get)): Action[AnyContent] = secureContext asyncAction { implicit request =>
    val atomic = isAtomic(default = true)

    (for {
      uuid <- id.uuidF
      model <- abstractDao.getSingle(uuid, atomic)
    } yield model).jsonResult(id)
  }
}
