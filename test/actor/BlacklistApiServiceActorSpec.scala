package actor

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import base.TestBaseDefinition
import dao.{AbstractDaoSpec, BlacklistDao}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatest.WordSpecLike
import org.scalatest.mockito.MockitoSugar.mock
import service.actor.NaturalDescribableYear
import service.blacklist.{BlacklistApiService, BlacklistApiServiceActor}

import scala.concurrent.Future
import scala.util.Success

class BlacklistApiServiceActorSpec
  extends TestKit(ActorSystem("BlacklistApiServiceActorSpec"))
    with WordSpecLike
    with TestBaseDefinition
    with ImplicitSender {

  import BlacklistApiServiceActor._

  val blackListDao = mock[BlacklistDao]
  val blacklistApiService = mock[BlacklistApiService]

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A SemesterCreationActorSpec" should {

    "create semesters by year (sync)" in {
      val blacklists = AbstractDaoSpec.populateBlacklists(5)
      val successfulBlacklists = blacklists.map(Success.apply)

      when(blacklistApiService.fetchLegalHolidays(any())(any())).thenReturn(Future.successful(blacklists))
      when(blackListDao.createManyPartial(any())).thenReturn(Future.successful(successfulBlacklists))

      val props = BlacklistApiServiceActor.props(blacklistApiService, blackListDao, Some(NaturalDescribableYear(2018)))
      val actor = system.actorOf(props)

      actor ! BlacklistDownloadRequestSync

      expectMsg(successfulBlacklists)
    }

    "create semesters by year (async)" in {
      val blacklists = AbstractDaoSpec.populateBlacklists(5)
      val successfulBlacklists = blacklists.map(Success.apply)

      when(blacklistApiService.fetchLegalHolidays(any())(any())).thenReturn(Future.successful(blacklists))
      when(blackListDao.createManyPartial(any())).thenReturn(Future.successful(successfulBlacklists))

      val props = BlacklistApiServiceActor.props(blacklistApiService, blackListDao, Some(NaturalDescribableYear(2018)))
      val actor = system.actorOf(props)

      actor ! BlacklistDownloadRequestAsync

      expectNoMessage()
    }

    "fail when year can't be parsed" in {
      val blacklists = AbstractDaoSpec.populateBlacklists(5)
      val successfulBlacklists = blacklists.map(Success.apply)

      when(blacklistApiService.fetchLegalHolidays(any())(any())).thenReturn(Future.successful(blacklists))
      when(blackListDao.createManyPartial(any())).thenReturn(Future.successful(successfulBlacklists))

      val props = BlacklistApiServiceActor.props(blacklistApiService, blackListDao, None)
      val actor = system.actorOf(props)

      actor ! BlacklistDownloadRequestSync

      expectMsgClass(classOf[Throwable])
    }

    "fail when apiService fails" in {
      when(blacklistApiService.fetchLegalHolidays(any())(any())).thenReturn(Future.failed(new Throwable("failed")))
      when(blackListDao.createManyPartial(any())).thenReturn(Future.successful(List.empty))

      val props = BlacklistApiServiceActor.props(blacklistApiService, blackListDao, Some(NaturalDescribableYear(2018)))
      val actor = system.actorOf(props)

      actor ! BlacklistDownloadRequestSync

      expectMsgClass(classOf[Throwable])
    }

    "fail when blacklistDao fails" in {
      val blacklists = AbstractDaoSpec.populateBlacklists(5)

      when(blacklistApiService.fetchLegalHolidays(any())(any())).thenReturn(Future.successful(blacklists))
      when(blackListDao.createManyPartial(any())).thenReturn(Future.failed(new Throwable("failed")))

      val props = BlacklistApiServiceActor.props(blacklistApiService, blackListDao, Some(NaturalDescribableYear(2018)))
      val actor = system.actorOf(props)

      actor ! BlacklistDownloadRequestSync

      expectMsgClass(classOf[Throwable])
    }
  }
}