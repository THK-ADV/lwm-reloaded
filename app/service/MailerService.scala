package service

import javax.inject.Inject
import play.api.libs.mailer._

import scala.concurrent.{ExecutionContext, Future}

class MailerService @Inject()(
  private val sender: Option[String],
  private val chunkSize: Option[Int],
  mailerClient: MailerClient,
  implicit val executionContext: ExecutionContext
) {

  def sendEmail(subject: String, body: String, bcc: Seq[String]): Future[Seq[String]] = (sender, chunkSize) match {
    case (Some(from), Some(chunk)) =>
      val template = makeEmail(subject, body, from) _

      Future {
        bcc.grouped(chunk).map(template andThen mailerClient.send).toList
      }
    case _ =>
      Future.failed(new Throwable("need to provide a sender and chunkSize in order to send emails"))
  }

  private def makeEmail(subject: String, body: String, from: String)(bcc: Seq[String]) = {
    Email(subject = subject, from = from, bcc = bcc, bodyText = Some(body))
  }
}