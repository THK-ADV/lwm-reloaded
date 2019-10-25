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

    "reorder indices at the beginning" in {
      val id = UUID.randomUUID
      val entries = List(
        service.dbModel(AssignmentEntryProtocol(id, "A", Set.empty, 1), 0, None),
        service.dbModel(AssignmentEntryProtocol(id, "B", Set.empty, 1), 1, None),
        service.dbModel(AssignmentEntryProtocol(id, "C", Set.empty, 1), 2, None),
      )

      val res = service.reorderIndices(entries, entries.head.id)
      res.size shouldBe 2
      res(0) shouldBe (entries(1).id, 0)
      res(1) shouldBe (entries(2).id, 1)
    }

    "reorder indices at the end" in {
      val id = UUID.randomUUID
      val entries = List(
        service.dbModel(AssignmentEntryProtocol(id, "A", Set.empty, 1), 0, None),
        service.dbModel(AssignmentEntryProtocol(id, "B", Set.empty, 1), 1, None),
        service.dbModel(AssignmentEntryProtocol(id, "C", Set.empty, 1), 2, None),
      )

      val res = service.reorderIndices(entries, entries.last.id)
      res.size shouldBe 2
      res(0) shouldBe (entries(0).id, 0)
      res(1) shouldBe (entries(1).id, 1)
    }

    "reorder indices in the middle" in {
      val id = UUID.randomUUID
      val entries = List(
        service.dbModel(AssignmentEntryProtocol(id, "A", Set.empty, 1), 0, None),
        service.dbModel(AssignmentEntryProtocol(id, "B", Set.empty, 1), 1, None),
        service.dbModel(AssignmentEntryProtocol(id, "C", Set.empty, 1), 2, None),
      )

      val res = service.reorderIndices(entries, entries(1).id)
      res.size shouldBe 2
      res(0) shouldBe (entries(0).id, 0)
      res(1) shouldBe (entries(2).id, 1)
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
      val others = service.reorderIndices(all, toDelete.id)

      when(dao.withSameLabworkAs(toDelete.id))
        .thenReturn(DBIO.successful(all))
      when(dao.invalidateSingle(toDelete.id))
        .thenReturn(DBIO.successful(toDelete))
      when(dao.updateIndices(others))
        .thenReturn(DBIO.successful(List(1, 1, 1)))
      when(dao.transaction[AssignmentEntry](any()))
        .thenReturn(Future.successful(toDelete.toUniqueEntity))

      async(service.invalidate(toDelete.id)) { deleted =>
        others.exists(_._2 == deleted.index) shouldBe true
        others.exists(_._1 == deleted.id) shouldBe false
      }
    }
  }
}
