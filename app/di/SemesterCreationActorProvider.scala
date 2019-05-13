package di

import akka.actor.{ActorRef, ActorSystem}
import javax.inject.{Inject, Provider, Singleton}
import play.api.Configuration
import service.SemesterService
import service.actor.{NaturalDescribableYear, SemesterCreationActor}

@Singleton
class SemesterCreationActorProvider @Inject()(
  private val system: ActorSystem,
  private val semesterService: SemesterService,
  private implicit val config: Configuration
) extends Provider[ActorRef] with ConfigReader {

  lazy val get = {
    val year = nonEmptyConfig("lwm.semester.year") flatMap NaturalDescribableYear.parse

    /* throwing providers seems to be complicated to support.
    thus, optionality has to be handled internally.
    see https://github.com/google/guice/wiki/ThrowingProviders */

    system.actorOf(SemesterCreationActor.props(semesterService, year))
  }
}