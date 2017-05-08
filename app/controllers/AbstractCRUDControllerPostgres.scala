package controllers

import java.sql.Timestamp
import java.util.UUID

import models.UniqueEntity
import play.api.libs.json.{JsError, JsValue, Reads, Writes}
import play.api.mvc.{Action, AnyContent, Controller, Request}
import services.AbstractDao
import slick.driver.PostgresDriver.api._
import store.{TableFilter, UniqueTable}

import scala.collection.Map
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

trait AttributeFilter {
  protected lazy val atomicAttribute = "atomic"
  protected lazy val validAttribute = "valid"
  protected lazy val lastModifiedAttribute = "lastModified"

  protected case class DefaultAttributes(atomic: Boolean = true, valid: Boolean = true, lastModified: Option[String] = None)

  private type QueryString = Map[String, Seq[String]]

  final protected def extractAttributes(queryString: QueryString): (QueryString, DefaultAttributes) = {
    def extractBool(seq: Seq[String], fallback: Boolean): Boolean = {
      seq.headOption.flatMap(s => Try(s.toBoolean).toOption).fold(fallback)(_ == true)
    }

    def extractTimestamp(seq: Seq[String]): Option[String] = {
      seq.headOption.flatMap(s => Try(s.toLong).flatMap(l => Try(new Timestamp(l))).toOption).map(_.getTime.toString)
    }

    var atomic = true
    var valid = true
    var lastModified: Option[String] = None

    val remaining = List(atomicAttribute, validAttribute, lastModifiedAttribute).foldLeft(queryString) {
      case (q, at) if at == atomicAttribute =>
        q.find(_._1 == at).fold(q) { seq =>
          atomic = extractBool(seq._2, fallback = false)
          q - at
        }
      case (q, inv) if inv == validAttribute =>
        q.find(_._1 == inv).fold(q) { seq =>
          valid = extractBool(seq._2, fallback = true)
          q - inv
        }
      case (q, mod) if mod == lastModifiedAttribute =>
        q.find(_._1 == mod).fold(q) { seq =>
          lastModified = extractTimestamp(seq._2)
          q - mod
        }
      case (q, _) => q
    }

    (remaining, DefaultAttributes(atomic, valid, lastModified))
  }
}

trait AbstractCRUDControllerPostgres[Protocol, T <: Table[DbModel] with UniqueTable, DbModel <: UniqueEntity, LwmModel <: UniqueEntity]
  extends Controller
    with Secured
    with SessionChecking
    with SecureControllerContext
    with ContentTyped
    with Chunked
    with PostgresResult
    with AttributeFilter
    with RequestRebasePostgres {

  import scala.concurrent.ExecutionContext.Implicits.global

  protected implicit def writes: Writes[LwmModel]
  protected implicit def reads: Reads[Protocol]

  protected def abstractDao: AbstractDao[T, DbModel, LwmModel]
  protected def tableFilter(attribute: String, values: Seq[String])(appendTo: Try[List[TableFilter[T]]]): Try[List[TableFilter[T]]]

  protected def toDbModel(protocol: Protocol, existingId: Option[UUID]): DbModel
  protected def toLwmModel(dbModel: DbModel): LwmModel

  final protected def parse[A](request: Request[JsValue])(implicit reads: Reads[A]): Try[A] = {
    request.body.validate[A].fold[Try[A]](
      errors => Failure(new Throwable(JsError.toJson(errors).toString())),
      success => Success(success)
    )
  }

  def create(secureContext: SecureContext = contextFrom(Create)): Action[JsValue] = secureContext asyncContentTypedAction { request =>
    (for {
      protocol <- Future.fromTry(parse[Protocol](request))
      dbModel = toDbModel(protocol, None)
      created <- abstractDao.create(dbModel)
    } yield toLwmModel(created)).jsonResult
  }

  def update(id: String, secureContext: SecureContext = contextFrom(Update)): Action[JsValue] = secureContext asyncContentTypedAction { request =>
    val uuid = UUID.fromString(id)

    (for {
      protocol <- Future.fromTry(parse[Protocol](request))
      dbModel = toDbModel(protocol, Some(uuid))
      updated <- abstractDao.update(dbModel)
    } yield updated.map(toLwmModel)).jsonResult(uuid)
  }

  def delete(id: String, secureContext: SecureContext = contextFrom(Delete)): Action[AnyContent] = secureContext asyncAction { _ =>
    val uuid = UUID.fromString(id)

    abstractDao.delete(uuid).map(_.map(toLwmModel)).jsonResult(uuid)
  }

  def all(secureContext: SecureContext = contextFrom(GetAll)): Action[AnyContent] = secureContext asyncAction { request =>
    val (queryString, defaults) = extractAttributes(request.queryString)

    val filter = queryString.foldLeft(Try(List.empty[TableFilter[T]])) {
      case (list, (attribute, values)) => tableFilter(attribute, values)(list)
    }

    (for{
      filter <- Future.fromTry(filter)
      results <- abstractDao.get(filter, defaults.atomic, defaults.valid, defaults.lastModified)
    } yield results).jsonResult
  }

  def get(id: String, secureContext: SecureContext = contextFrom(Get)): Action[AnyContent] = secureContext asyncAction { request =>
    val atomic = extractAttributes(request.queryString)._2.atomic

    abstractDao.getById(id, atomic).jsonResult(id)
  }
}