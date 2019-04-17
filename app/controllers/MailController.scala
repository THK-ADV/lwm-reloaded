package controllers

import java.util.UUID

import controllers.helper.{JsonParser, ResultOps, SecureControllerContext, Secured}
import dao.{AuthorityDao, ReportCardEntryDao}
import javax.inject.{Inject, Singleton}
import models.Role.{CourseManager, God}
import play.api.libs.json.{Json, Reads}
import play.api.mvc.{AbstractController, ControllerComponents}
import security.SecurityActionChain
import service.MailerService

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

@Singleton
class MailController @Inject()(
  cc: ControllerComponents,
  mailerService: MailerService,
  val reportCardEntryDao: ReportCardEntryDao,
  val authorityDao: AuthorityDao,
  val securedAction: SecurityActionChain,
  implicit val context: ExecutionContext
) extends AbstractController(cc)
  with Secured
  with SecureControllerContext
  with JsonParser
  with ResultOps {

  private case class MailProtocol(subject: String, body: String, recipients: Seq[String], replyTo: Option[String])

  private implicit val reads: Reads[MailProtocol] = Json.reads[MailProtocol]

  def sendMailWithBody(course: String) = restrictedContext(course)(Create) action { request =>
    val result = for {
      protocol <- parseJson(request)(reads)
      id <- mailerService.sendEmail(protocol.subject, protocol.body, protocol.recipients, protocol.replyTo)
    } yield (id, protocol.recipients)

    result match {
      case Success((id, recipients)) => makeOk(id, recipients)
      case Failure(e) => internalServerError(e)
    }
  }

  def sendMailToAttendeesOf(course: String, labwork: String) = restrictedContext(course)(Create) asyncAction { request =>
    val result = for {
      protocol <- Future.fromTry(parseJson(request)(reads))
      labworkId <- Future.fromTry(Try(UUID.fromString(labwork)))
      studentMails <- reportCardEntryDao.attendeeEmailAddressesOf(labworkId)
      id <- Future.fromTry(mailerService.sendEmail(protocol.subject, protocol.body, studentMails, protocol.replyTo))
    } yield (id, studentMails)

    result.jsonResult(t => makeOk(t._1, t._2))
  }

  private def makeOk(id: String, recipients: Seq[String]) = Ok(Json.obj(
    "message" -> s"Email $id send",
    "recipients" -> recipients
  ))

  override protected def restrictedContext(restrictionId: String): PartialFunction[Rule, SecureContext] = {
    case Create => SecureBlock(restrictionId, List(CourseManager))
    case _ => PartialSecureBlock(List(God))
  }

  override protected def contextFrom: PartialFunction[Rule, SecureContext] = forbiddenAction()
}
