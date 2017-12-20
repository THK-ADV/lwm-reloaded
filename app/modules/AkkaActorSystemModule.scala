package modules

import akka.actor.ActorSystem

trait AkkaActorSystemModule {
  implicit lazy val system: ActorSystem = ActorSystem("lwm-system")
}
