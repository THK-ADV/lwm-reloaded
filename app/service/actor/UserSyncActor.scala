package service.actor

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import models.User
import service.UserSyncService

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

object UserSyncActor {

  def sync(
      system: ActorSystem,
      userSyncService: UserSyncService,
      users: Seq[User]
  ): Unit = {
    val a = system.actorOf(Props(new SyncActor(userSyncService)))
    val b = system.actorOf(Props(new UserSyncActor(a, 5.seconds)))
    b ! Go(users.toList)
  }

  private case class Go(users: List[User])
  private case object Finish
  private case class Sync(user: User)

  private implicit val timeout: Timeout = Timeout(5.seconds)

  private def await[A](f: Future[A]) =
    Try(Await.result(f, timeout.duration))

  private class UserSyncActor(
      private val syncActor: ActorRef,
      private val scheduleInterval: FiniteDuration
  ) extends Actor
      with ActorLogging {

    import context.dispatcher

    override def receive = { case Go(users) =>
      println("user sync started")
      sync(users)
    }

    private def sync(users: List[User]): Unit = {
      def go(u: User, xs: List[User]) =
        context.system.scheduler.scheduleOnce(scheduleInterval) {
          await(syncActor ? Sync(u))
          sync(xs)
        }

      users match {
        case h :: t =>
          go(h, t)
        case Nil =>
          syncActor ! Finish
          context.stop(self)
      }
    }
  }

  private class SyncActor(private val userSyncService: UserSyncService)
      extends Actor
      with ActorLogging {

    override def receive = {
      case Sync(user) =>
        println(s"syncing ${user.systemId} ...")
        sync(user)
        sender ! Unit
      case Finish =>
        println("user sync finished")
        context.stop(self)
    }

    private def sync(user: User): Unit = {
      val task = userSyncService.fetchAndUpdateUser(user)
      await(task) match {
        case Success(res) =>
          println(s"previous: ${res.previous}")
          println(s"now: ${res.updated}")
        case Failure(err) =>
          println(
            s"user sync failed for user $user with error: ${err.getMessage}"
          )
      }
      println("===")
    }
  }
}
