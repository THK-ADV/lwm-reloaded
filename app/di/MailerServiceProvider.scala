package di

import javax.inject.{Inject, Provider}
import play.api.Configuration
import play.api.libs.mailer.MailerClient
import service.MailerService

class MailerServiceProvider @Inject()(config: Configuration, mailerClient: MailerClient) extends Provider[MailerService] {

  lazy val get = {
    val sender = config getOptional[String] "lwm.mail.sender"
    new MailerService(sender, mailerClient)
  }
}
