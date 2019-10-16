package service.actor

import akka.actor.{Actor, Props}
import service.actor.ReportCardMigrationAktor.MigrationRequest

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object ReportCardMigrationAktor {

  trait MigrationRequest[A] {
    def name: String

    def f: () => Future[Seq[A]]
  }

  def props(implicit ctx: ExecutionContext) = Props(new ReportCardMigrationAktor())
}

class ReportCardMigrationAktor(implicit val ctx: ExecutionContext) extends Actor {

  override def receive = {
    case x: MigrationRequest[_] =>
      println(s"migrating ${x.name}")

      x.f() onComplete {
        case Success(xs) =>
          println(s"successfully migrated ${xs.size} ${x.name}")
        case Failure(throwable) =>
          println(s"failed to migrate ${x.name} with throwable: ${throwable.getLocalizedMessage}")
      }
    case _ =>
      println("unknown request")
  }
}
