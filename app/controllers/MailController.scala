package controllers

import controllers.helper.{JsonParser, ResultOps, SecureControllerContext, Secured}
import dao.{AuthorityDao, LabworkDao, ReportCardEntryDao}
import models.LabworkAtom
import play.api.libs.json.{Json, Reads}
import play.api.mvc.{AbstractController, ControllerComponents}
import security.LWMRole.{CourseManager, God}
import security.SecurityActionChain
import service.MailerService

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@Singleton
class MailController @Inject()(
  cc: ControllerComponents,
  mailerService: MailerService,
  val reportCardEntryDao: ReportCardEntryDao,
  val labworkDao: LabworkDao,
  val authorityDao: AuthorityDao,
  val securedAction: SecurityActionChain,
  implicit val context: ExecutionContext
) extends AbstractController(cc)
  with Secured
  with SecureControllerContext
  with JsonParser
  with ResultOps {

  private case class SimpleMailProtocol(subject: String, body: String, bcc: Seq[String])

  private implicit val reads: Reads[SimpleMailProtocol] = Json.reads[SimpleMailProtocol]

  def sendMailWithBody(course: String) = restrictedContext(course)(Create) asyncAction { request =>
    val result = for {
      protocol <- Future.fromTry(parseJson(request)(reads))
      ids <- mailerService.sendEmail(protocol.subject, protocol.body, protocol.bcc)
    } yield (protocol.bcc, ids)

    result.jsonResult(t => makeOk(t._1, t._2))
  }

  def sendMailToAttendeesOf(course: String, labwork: String) = restrictedContext(course)(Create) asyncAction { request =>
    val result = for {
      protocol <- Future.fromTry(parseJson(request)(reads))
      labworkId <- Future.fromTry(Try(UUID.fromString(labwork)))
      maybeLabwork <- labworkDao.getSingle(labworkId) if maybeLabwork.isDefined
      lab = maybeLabwork.get.asInstanceOf[LabworkAtom]
      studentMails <- reportCardEntryDao.attendeeEmailAddressesOf(labworkId)
      recipients = studentMails ++ protocol.bcc
      ids <- mailerService.sendEmail(s"[${lab.label} ${lab.semester.abbreviation}] ${protocol.subject}", protocol.body, recipients)
    } yield (recipients, ids)

    result.jsonResult(t => makeOk(t._1, t._2))
  }

  private def makeOk(recipients: Seq[String], ids: Seq[String]) = Ok(Json.obj(
    "ids" -> ids.map(i => s"Email $i send"),
    "bcc" -> recipients
  ))

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = forbiddenAction()
}
