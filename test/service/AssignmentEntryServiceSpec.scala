package service

import java.util.UUID

import base.{AsyncSpec, TestBaseDefinition}
import dao.AssignmentEntryDao
import models.{AssignmentEntry, AssignmentEntryProtocol, AssignmentEntryType}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito._
import org.scalatest.WordSpec
import org.scalatest.mockito.MockitoSugar

import scala.concurrent.Future

class AssignmentEntryServiceSpec extends WordSpec with TestBaseDefinition with MockitoSugar with AsyncSpec {

  import slick.jdbc.PostgresProfile.api._

  import scala.concurrent.ExecutionContext.Implicits.global

  val dao = mock[AssignmentEntryDao]
  val service = new AssignmentEntryServiceImpl(dao, global)

  "A AssignmentEntryServiceSpec" should {

    "create a fresh assignment entry" in {
      val id = UUID.randomUUID
      val p = AssignmentEntryProtocol(UUID.randomUUID, "A", Set(AssignmentEntryType.Attendance), 1)
      val i = 0

      when(dao.count(any(), any(), any()))
        .thenReturn(Future.successful(i))
      when(dao.create(any()))
        .thenReturn(Future.successful(service.dbModel(p, i, Some(id))))

      async(service.create(p)) { e =>
        e.labwork shouldBe p.labwork
        e.label shouldBe p.label
        e.index shouldBe 0
        e.duration shouldBe 1
        e.types shouldBe p.types
        e.id shouldBe id
      }
    }

    "create yet another assignment entry" in {
      val id = UUID.randomUUID
      val p = AssignmentEntryProtocol(UUID.randomUUID, "A", Set(AssignmentEntryType.Attendance), 1)
      val i = 3

      when(dao.count(any(), any(), any()))
        .thenReturn(Future.successful(i))
      when(dao.create(any()))
        .thenReturn(Future.successful(service.dbModel(p, i, Some(id))))

      async(service.create(p)) { e =>
        e.labwork shouldBe p.labwork
        e.label shouldBe p.label
        e.index shouldBe 3
        e.duration shouldBe 1
        e.types shouldBe p.types
        e.id shouldBe id
      }
    }

    "reorder assignment entries correctly" in {
      val id = UUID.randomUUID
      val entries = (0 until 10).map { i =>
        service.dbModel(AssignmentEntryProtocol(id, i.toString, Set(AssignmentEntryType.Attendance), 1), i, None)
      }.toList

      val res = service.reorder(entries, entries(2).id)
      res.size shouldBe 9
      (0 until 9).forall(i => res(i).index == i) shouldBe true
    }

    "invalidate an assignment entry and adjust all indices afterwards" in {
      val labwork = UUID.randomUUID
      val all = List(
        AssignmentEntryProtocol(labwork, "A", Set(AssignmentEntryType.Attendance), 1),
        AssignmentEntryProtocol(labwork, "B", Set(AssignmentEntryType.Attendance), 1),
        AssignmentEntryProtocol(labwork, "C", Set(AssignmentEntryType.Attendance), 1),
        AssignmentEntryProtocol(labwork, "D", Set(AssignmentEntryType.Attendance), 1)
      ).zipWithIndex.map(t => service.dbModel(t._1, t._2, None))
      val toDelete = all(1)
      val others = service.reorder(all, toDelete.id)

      when(dao.withSameLabworkAs(toDelete.id))
        .thenReturn(DBIO.successful(all))
      when(dao.invalidateSingle(toDelete.id))
        .thenReturn(DBIO.successful(toDelete))
      when(dao.updateIndices(others.map(t => (t.id, t.index))))
        .thenReturn(DBIO.successful(List(1, 1, 1)))
      when(dao.transaction[List[AssignmentEntry]](any()))
        .thenReturn(Future.successful(others.map(_.toUniqueEntity)))

      async(service.invalidate(toDelete.id)) { existing =>
        existing(0).index shouldBe 0
        existing(1).index shouldBe 1
        existing(2).index shouldBe 2
      }
    }
  }
}
