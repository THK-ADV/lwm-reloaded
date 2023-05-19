package controllers

import dao._
import dao.helper.TableFilter.labworkFilter
import database.{LabworkApplicationDb, LabworkApplicationTable}
import models._
import play.api.libs.json.{Json, Reads, Writes}
import play.api.mvc.{AnyContent, ControllerComponents, Request, Result}
import security.LWMRole._
import security.SecurityActionChain
import service.sheet.{ApplicantExport, FileStreamResult}
import utils.Ops.OptionOps

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Try}

object LabworkApplicationController {
  lazy val labworkAttribute = "labwork"
  lazy val courseAttribute = "course"
  lazy val applicantAttribute = "applicant"
}

@Singleton
final class LabworkApplicationController @Inject() (
    cc: ControllerComponents,
    val authorityDao: AuthorityDao,
    val abstractDao: LabworkApplicationDao,
    val userDao: UserDao,
    val securedAction: SecurityActionChain,
    implicit val ctx: ExecutionContext
) extends AbstractCRUDController[
      LabworkApplicationProtocol,
      LabworkApplicationTable,
      LabworkApplicationDb,
      LabworkApplicationLike
    ](cc)
    with FileStreamResult {

  import LabworkApplicationController._

  override protected implicit val writes: Writes[LabworkApplicationLike] =
    LabworkApplicationLike.writes

  override protected implicit val reads: Reads[LabworkApplicationProtocol] =
    LabworkApplicationProtocol.reads

  private def ensureIsStudent(
      k: () => Future[Result]
  )(implicit request: Request[AnyContent]): Future[Result] =
    for {
      systemId <- Future.fromTry(
        request.systemId.toTry("No User ID found in request")
      )
      student <- userDao.getBySystemId(systemId, atomic = false)
      protocol <- Future.fromTry(parseJson(request)(reads))
      res <- student match {
        case Some(student) if student.id == protocol.applicant => k()
        case Some(student) =>
          Future.successful(
            Unauthorized(
              Json.obj(
                "status" -> "KO",
                "message" -> s"${student.systemId} is unauthorized for the current action"
              )
            )
          )
        case None =>
          Future.failed(new Throwable("No Student ID found in request"))
      }
    } yield res

  override def create(secureContext: SecureContext) =
    secureContext asyncAction { implicit request =>
      ensureIsStudent(() => super.create(NonSecureBlock)(request))
    }

  def createFrom(course: String) =
    restrictedContext(course)(Create) asyncAction { request =>
      super.create(NonSecureBlock)(request)
    }

  override def update(id: String, secureContext: SecureContext) =
    secureContext asyncAction { implicit request =>
      ensureIsStudent(() => super.update(id, NonSecureBlock)(request))
    }

  def updateFrom(course: String, id: String) =
    restrictedContext(course)(Update) asyncAction { request =>
      super.update(id, NonSecureBlock)(request)
    }

  override def invalidate(id: String, secureContext: SecureContext) =
    secureContext asyncAction { implicit request =>
      ensureIsStudent(() => super.invalidate(id, secureContext)(request))
    }

  def invalidateFrom(course: String, id: String) =
    restrictedContext(course)(Delete) asyncAction { request =>
      super.invalidate(id, NonSecureBlock)(request)
    }

  override def get(id: String, secureContext: SecureContext) =
    secureContext asyncAction { implicit request =>
      ensureIsStudent(() => super.get(id, secureContext)(request))
    }

  def allFrom(course: String, labwork: String) =
    restrictedContext(course)(GetAll) asyncAction { implicit request =>
      super.all(NonSecureBlock)(
        request.appending(
          courseAttribute -> Seq(course),
          labworkAttribute -> Seq(labwork)
        )
      )
    }

  def countFrom(course: String, labwork: String) =
    restrictedContext(course)(GetAll) asyncAction { implicit request =>
      count(NonSecureBlock)(
        request.appending(
          courseAttribute -> Seq(course),
          labworkAttribute -> Seq(labwork)
        )
      )
    }

  def renderApplicantSheet(course: String, labwork: String) =
    restrictedContext(course)(Get) asyncAction { implicit request =>
      import utils.Ops.whenNonEmpty

      for {
        labworkId <- labwork.uuidF
        apps <- whenNonEmpty(abstractDao.get(List(labworkFilter(labworkId))))(
          () => "no applications found"
        )
        atoms = apps.map(_.asInstanceOf[LabworkApplicationAtom]).toList
        sheet = ApplicantExport.createSheet(atoms, atoms.head.labwork)
      } yield toResult(sheet)
    }

  override protected def makeTableFilter(
      attribute: String,
      value: String
  ): Try[TableFilterPredicate] =
    (attribute, value) match {
      case (`labworkAttribute`, l)   => l.makeLabworkFilter
      case (`courseAttribute`, c)    => c.makeCourseFilter
      case (`applicantAttribute`, a) => a.makeUserFilter
      case _                         => Failure(new Throwable(s"Unknown attribute $attribute"))
    }

  override protected def toDbModel(
      protocol: LabworkApplicationProtocol,
      existingId: Option[UUID]
  ): LabworkApplicationDb =
    LabworkApplicationDb(
      protocol.labwork,
      protocol.applicant,
      protocol.friends,
      id = existingId getOrElse UUID.randomUUID
    )

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Create => PartialSecureBlock(List(StudentRole))
    case Update => PartialSecureBlock(List(StudentRole))
    case Delete => PartialSecureBlock(List(StudentRole))
    case Get    => PartialSecureBlock(List(StudentRole))
    case GetAll => PartialSecureBlock(List(Admin))
  }

  override protected def restrictedContext(
      restrictionId: String
  ): PartialFunction[Rule, SecureContext] = {
    case GetAll =>
      SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case Get    => SecureBlock(restrictionId, List(CourseManager))
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case Update => SecureBlock(restrictionId, List(CourseManager))
    case Delete => SecureBlock(restrictionId, List(CourseManager))
    case _      => PartialSecureBlock(List(God))
  }
}
