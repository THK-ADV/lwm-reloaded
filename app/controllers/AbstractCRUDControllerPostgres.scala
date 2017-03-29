package controllers

import java.util.UUID

import models.UniqueEntity
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.Controller
import services.AbstractDao
import slick.driver.PostgresDriver.api._
import store.{TableFilter, UniqueTable}

import scala.concurrent.Future
import scala.util.Try

trait AbstractCRUDControllerPostgres[Protocol, T <: Table[DbModel] with UniqueTable, DbModel <: UniqueEntity, LwmModel <: UniqueEntity]
  extends Controller
    with Secured
    with SessionChecking
    with SecureControllerContext
    with ContentTyped
    with Chunked
    with PostgresResult {

  import scala.concurrent.ExecutionContext.Implicits.global

  protected implicit def writes: Writes[LwmModel]
  protected implicit def reads: Reads[Protocol]

  protected def abstractDao: AbstractDao[T, DbModel, LwmModel]
  protected def idTableFilter(id: String): TableFilter[T]
  protected def tableFilter(attribute: String, values: Seq[String])(appendTo: Try[List[TableFilter[T]]]): Try[List[TableFilter[T]]]

  protected def toDbModel(protocol: Protocol, existingId: Option[UUID]): DbModel
  protected def toLwmModel(dbModel: DbModel): LwmModel

  def create = contextFrom(Create) asyncContentTypedAction { request =>
    (for {
      protocol <- Future.fromTry(parse[Protocol](request))
      dbModel = toDbModel(protocol, None)
      created <- abstractDao.create(dbModel)
    } yield toLwmModel(created)).jsonResult
  }

  def update(id: String) = contextFrom(Update) asyncContentTypedAction { request =>
    val uuid = UUID.fromString(id)

    (for {
      protocol <- Future.fromTry(parse[Protocol](request))
      dbModel = toDbModel(protocol, Some(uuid))
      updated <- abstractDao.update(dbModel)
    } yield updated.map(toLwmModel)).jsonResult(uuid)
  }

  def delete(id: String) = contextFrom(Delete) asyncAction { _ =>
    val uuid = UUID.fromString(id)

    abstractDao.delete(uuid).map(_.map(toLwmModel)).jsonResult(uuid)
  }

  def all = contextFrom(GetAll) asyncAction { request =>
    val (queryString, atomic) = extractAtomic(request.queryString)

    val filter = queryString.foldLeft(Try(List.empty[TableFilter[T]])) {
      case (list, (attribute, values)) => tableFilter(attribute, values)(list)
    }

    (for{
      filter <- Future.fromTry(filter)
      results <- abstractDao.get(filter, atomic)
    } yield results).jsonResult
  }

  def get(id: String) = contextFrom(Get) asyncAction { request =>
    val atomic = extractAtomic(request.queryString)._2

    abstractDao.get(List(idTableFilter(id)), atomic).map(_.headOption).jsonResult(id)
  }
}