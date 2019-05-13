package actor

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import base.TestBaseDefinition
import dao.{AbstractDaoSpec, SemesterDao}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatest.WordSpecLike
import org.scalatest.mockito.MockitoSugar._
import service.SemesterService
import service.actor.SemesterCreationActor.{CreationRequestAsync, CreationRequestSync}
import service.actor.{NaturalDescribableYear, SemesterCreationActor}

import scala.concurrent.Future
import scala.util.Success

class SemesterCreationActorSpec
  extends TestKit(ActorSystem("SemesterCreationActorSpec"))
    with WordSpecLike
    with TestBaseDefinition
    with ImplicitSender {

  val semesterDao = mock[SemesterDao]
  val semesterService = new SemesterService(semesterDao)

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A SemesterCreationActorSpec" should {

    "create semesters by year (sync)" in {
      val semesters = AbstractDaoSpec.populateSemester(2).map(Success.apply)
      when(semesterDao.createManyPartial(any())).thenReturn(Future.successful(semesters))

      val props = SemesterCreationActor.props(semesterService, Some(NaturalDescribableYear(2018)))
      val actor = system.actorOf(props)

      actor ! CreationRequestSync

      expectMsg(semesters)
    }

    "create semesters by year (async)" in {
      val semesters = AbstractDaoSpec.populateSemester(2).map(Success.apply)
      when(semesterDao.createManyPartial(any())).thenReturn(Future.successful(semesters))

      val props = SemesterCreationActor.props(semesterService, Some(NaturalDescribableYear(2018)))
      val actor = system.actorOf(props)

      actor ! CreationRequestAsync

      expectNoMessage()
    }

    "fail when year can't be parsed" in {
      when(semesterDao.createManyPartial(any())).thenReturn(Future.successful(List.empty))

      val props = SemesterCreationActor.props(semesterService, None)
      val actor = system.actorOf(props)

      actor ! CreationRequestSync

      expectMsgClass(classOf[Throwable])
    }

    "fail when semesterDao fails" in {
      when(semesterDao.createManyPartial(any())).thenReturn(Future.failed(new Throwable("fail")))

      val props = SemesterCreationActor.props(semesterService, Some(NaturalDescribableYear(2018)))
      val actor = system.actorOf(props)

      actor ! CreationRequestSync

      expectMsgClass(classOf[Throwable])
    }
  }
}
