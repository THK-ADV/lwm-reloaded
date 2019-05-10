package di

import akka.actor.{ActorRef, ActorSystem}
import dao.BlacklistDao
import javax.inject.{Inject, Provider, Singleton}
import play.api.Configuration
import service.actor.NaturalDescribableYear
import service.blacklist.{BlacklistApiService, BlacklistApiServiceActor}

@Singleton
class BlacklistApiServiceActorProvider @Inject()(
  private val system: ActorSystem,
  private val blacklistApiService: BlacklistApiService,
  private val blacklistDao: BlacklistDao,
  private implicit val config: Configuration
) extends Provider[ActorRef] with ConfigReader {

  lazy val get = {
    val year = nonEmptyConfig("lwm.blacklist.year") flatMap NaturalDescribableYear.parse

    /* throwing providers seems to be complicated to support.
    thus, optionality has to be handled internally.
    see https://github.com/google/guice/wiki/ThrowingProviders */

    system.actorOf(BlacklistApiServiceActor.props(blacklistApiService, blacklistDao, year))
  }
}