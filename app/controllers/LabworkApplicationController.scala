package controllers

import dao._
import dao.helper.TableFilter.labworkFilter
import database.{LabworkApplicationDb, LabworkApplicationTable}
import models._
import play.api.libs.json.{Reads, Writes}
import play.api.mvc.ControllerComponents
import security.LWMRole._
import security.SecurityActionChain
import service.sheet.{ApplicantExport, FileStreamResult}

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Try}

object LabworkApplicationController {
  lazy val labworkAttribute = "labwork"
  lazy val courseAttribute = "course"
  lazy val applicantAttribute = "applicant"
}

@Singleton
final class LabworkApplicationController @Inject()(
  cc: ControllerComponents,
  val authorityDao: AuthorityDao,
  val abstractDao: LabworkApplicationDao,
  val securedAction: SecurityActionChain,
  implicit val ctx: ExecutionContext
) extends AbstractCRUDController[LabworkApplicationProtocol, LabworkApplicationTable, LabworkApplicationDb, LabworkApplicationLike](cc)
  with FileStreamResult {

  import LabworkApplicationController._

  override protected implicit val writes: Writes[LabworkApplicationLike] = LabworkApplicationLike.writes

  override protected implicit val reads: Reads[LabworkApplicationProtocol] = LabworkApplicationProtocol.reads

  def countFrom(course: String, labwork: String) = restrictedContext(course)(GetAll) asyncAction { implicit request =>
    count(NonSecureBlock)(request.appending(courseAttribute -> Seq(course), labworkAttribute -> Seq(labwork)))
  }

  def renderApplicantSheet(course: String, labwork: String) = restrictedContext(course)(Get) asyncAction { implicit request =>
    import utils.Ops.whenNonEmpty

    for {
      labworkId <- labwork.uuidF
      apps <- whenNonEmpty(abstractDao.get(List(labworkFilter(labworkId))))(() => "no applications found")
      atoms = apps.map(_.asInstanceOf[LabworkApplicationAtom]).toList
      sheet = ApplicantExport.createSheet(atoms, atoms.head.labwork)
    } yield toResult(sheet)
  }

  override protected def makeTableFilter(attribute: String, value: String): Try[TableFilterPredicate] =
    (attribute, value) match {
      case (`labworkAttribute`, l) => l.makeLabworkFilter
      case (`courseAttribute`, c) => c.makeCourseFilter
      case (`applicantAttribute`, a) => a.makeUserFilter
      case _ => Failure(new Throwable(s"Unknown attribute $attribute"))
    }

  override protected def toDbModel(protocol: LabworkApplicationProtocol, existingId: Option[UUID]): LabworkApplicationDb =
    LabworkApplicationDb(protocol.labwork, protocol.applicant, protocol.friends, id = existingId getOrElse UUID.randomUUID)

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = {
    case Create => PartialSecureBlock(List(StudentRole))
    case Update => PartialSecureBlock(List(StudentRole))
    case Delete => PartialSecureBlock(List(StudentRole))
    case Get => PartialSecureBlock(List(StudentRole))
    case GetAll => PartialSecureBlock(List(StudentRole, EmployeeRole))
  }

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case GetAll => SecureBlock(restrictionId, List(CourseManager, CourseEmployee))
    case Get => SecureBlock(restrictionId, List(CourseManager))
    case _ => PartialSecureBlock(List(God))
  }
}
