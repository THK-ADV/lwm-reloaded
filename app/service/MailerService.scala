package service

import javax.inject.Inject
import play.api.libs.mailer._

import scala.util.{Failure, Success, Try}

class MailerService @Inject()(private val sender: Option[String], mailerClient: MailerClient) {

  def sendEmail(subject: String, body: String, recipients: Seq[String], replyTo: Option[String]): Try[String] = sender match {
    case Some(from) =>
      val email = Email(
        subject = subject,
        from = from,
        bcc = recipients,
        replyTo = replyTo.toSeq,
        bodyText = Some(body),
      )

      Success(mailerClient send email)
    case None =>
      Failure(new Throwable("need to provide a sender in order to send emails"))
  }
}