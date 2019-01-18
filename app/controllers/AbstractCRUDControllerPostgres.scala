package controllers

import java.util.UUID

import controllers.helper._
import dao.AbstractDao
import javax.inject.Inject
import models.{UniqueDbEntity, UniqueEntity}
import play.api.libs.json._
import play.api.mvc._
import slick.jdbc.PostgresProfile.api._
import store.{TableFilter, UniqueTable}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

abstract class AbstractCRUDControllerPostgres[Protocol, T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity] @Inject()(cc: ControllerComponents) extends AbstractController(cc)
  with Secured
  with SecureControllerContext
  with PostgresResult
  with AttributeFilter
  with RequestRebase {

  import utils.Ops.unwrapTrys

  import scala.concurrent.ExecutionContext.Implicits.global

  protected implicit def writes: Writes[LwmModel]

  protected implicit def reads: Reads[Protocol]

  implicit def listReads[R](implicit r: Reads[R]): Reads[List[R]] = Reads.list[R]

  protected def abstractDao: AbstractDao[T, DbModel, LwmModel]

  protected def tableFilter(attribute: String, value: String)(appendTo: Try[List[TableFilter[T]]]): Try[List[TableFilter[T]]]

  protected def toDbModel(protocol: Protocol, existingId: Option[UUID]): DbModel

  protected def toLwmModel(dbModels: TraversableOnce[DbModel]): Seq[LwmModel] = dbModels.map(_.toLwmModel.asInstanceOf[LwmModel]).toSeq

  final protected def parseJson(request: Request[AnyContent])(implicit reads: Reads[Protocol]): Try[Protocol] = unwrap(request).flatMap(js => validate(js)(reads))

  final protected def parseJsonArray(request: Request[AnyContent])(implicit reads: Reads[List[Protocol]]): Try[List[Protocol]] = unwrap(request).flatMap(js => validate(js)(reads))

  private def validate[A](json: JsValue)(implicit reads: Reads[A]) = json.validate[A].fold[Try[A]](
    errors => Failure(new Throwable(JsError.toJson(errors).toString)),
    success => Success(success)
  )

  private def unwrap(request: Request[AnyContent]) = request.body.asJson match {
    case Some(json) => Success(json)
    case None => Failure(new Throwable("no json body"))
  }

  def create(secureContext: SecureContext = contextFrom(Create)) = secureContext asyncAction { request =>
    val atomic = extractAttributes(request.queryString, defaultAtomic = false)._2.atomic

    (for {
      protocols <- Future.fromTry(parseJsonArray(request))
      dbModels = protocols.map(p => toDbModel(p, None))
      partialCreated <- abstractDao.createManyPartial(dbModels)
      (succeeded, failed) = unwrapTrys(partialCreated)
      lwmModel <- if (atomic)
        abstractDao.getMany(succeeded.map(_.id), atomic)
      else
        Future.successful(toLwmModel(succeeded))
    } yield (toLwmModel(dbModels), lwmModel, failed)).jsonResult
  }

  def update(id: String, secureContext: SecureContext = contextFrom(Update)) = secureContext asyncAction { request =>
    val uuid = UUID.fromString(id)
    val atomic = extractAttributes(request.queryString, defaultAtomic = false)._2.atomic

    (for {
      protocol <- Future.fromTry(parseJson(request))
      dbModel = toDbModel(protocol, Some(uuid))
      updated <- abstractDao.update(dbModel)
      lwmModel <- if (atomic)
        abstractDao.getById(uuid.toString, atomic)
      else
        Future.successful(updated.map(_.toLwmModel.asInstanceOf[LwmModel]))
    } yield lwmModel).jsonResult(uuid)
  }

  def delete(id: String, secureContext: SecureContext = contextFrom(Delete)) = secureContext asyncAction { _ =>
    delete0(UUID.fromString(id))
  }

  protected def delete0(uuid: UUID): Future[Result] = {
    import utils.LwmDateTime.{SqlTimestampConverter, writeDateTime}

    abstractDao.delete(uuid).map(_.map(_.dateTime)).jsonResult(uuid)
  }

  def all(secureContext: SecureContext = contextFrom(GetAll)) = secureContext asyncAction { request =>
    val (queryString, defaults) = extractAttributes(request.queryString)

    val filter = queryString.foldLeft(Try(List.empty[TableFilter[T]])) {
      case (list, (attribute, values)) => tableFilter(attribute, values.head)(list)
    }

    (for {
      filter <- Future.fromTry(filter)
      results <- abstractDao.get(filter, defaults.atomic, defaults.valid, defaults.lastModified)
    } yield results).jsonResult
  }

  def get(id: String, secureContext: SecureContext = contextFrom(Get))  = secureContext asyncAction { request =>
    val atomic = extractAttributes(request.queryString)._2.atomic

    abstractDao.getById(id, atomic).jsonResult(id)
  }
}