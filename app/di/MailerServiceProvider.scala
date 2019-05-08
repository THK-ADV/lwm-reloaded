package di

import javax.inject.{Inject, Provider}
import play.api.Configuration
import play.api.libs.mailer.MailerClient
import service.MailerService

import scala.concurrent.ExecutionContext

class MailerServiceProvider @Inject()(config: Configuration, mailerClient: MailerClient, executionContext: ExecutionContext) extends Provider[MailerService] {

  lazy val get = {
    val sender = config getOptional[String] "lwm.mail.sender"
    val chunkSize = config getOptional[Int] "lwm.mail.chunkSize"

    new MailerService(sender, chunkSize, mailerClient, executionContext)
  }
}
