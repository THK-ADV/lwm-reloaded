package modules

import akka.actor.ActorSystem

trait AkkaActorSystemModule {
  implicit val system: ActorSystem = ActorSystem("lwm-system")
}
