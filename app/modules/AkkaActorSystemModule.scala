package modules

import akka.actor.ActorSystem


trait AkkaActorSystemModule {

  implicit def system = ActorSystem("lwm-system")
}
