package service

import java.util.UUID

import dao.AssignmentEntryDao
import database.{AssignmentEntryDb, AssignmentEntryTypeDb}
import javax.inject.Inject
import models.{AssignmentEntry, AssignmentEntryAtom, AssignmentEntryLike, AssignmentEntryProtocol}

import scala.concurrent.{ExecutionContext, Future}

trait AssignmentEntryService {
  implicit def ctx: ExecutionContext

  def dao: AssignmentEntryDao

  def takeover(srcLabwork: UUID, destLabwork: UUID): Future[Seq[AssignmentEntry]] = {
    def toDbModel(e: AssignmentEntry): AssignmentEntryDb = {
      val id = UUID.randomUUID

      AssignmentEntryDb(
        destLabwork,
        e.index,
        e.label,
        e.types.map(t => AssignmentEntryTypeDb(id, t.entryType)),
        e.duration,
        id = id
      )
    }

    def toAssignmentEntry(e: AssignmentEntryLike): AssignmentEntry = e.asInstanceOf[AssignmentEntry]

    for {
      current <- dao.count(List(AssignmentEntryDao.labworkFilter(destLabwork))) if current == 0
      template <- dao.get(List(AssignmentEntryDao.labworkFilter(srcLabwork)), atomic = false) if template.nonEmpty
      entries = template.map(toAssignmentEntry _ andThen toDbModel)
      result <- dao.createMany(entries.toList)
    } yield result.map(_.toUniqueEntity)
  }

  def create(p: AssignmentEntryProtocol): Future[AssignmentEntry] = {
    for {
      i <- dao.count(List(AssignmentEntryDao.labworkFilter(p.labwork)))
      m = dbModel(p, i, None)
      c <- dao.create(m)
    } yield c.toUniqueEntity
  }

  def update(id: UUID, p: AssignmentEntryProtocol): Future[AssignmentEntryLike] = {
    for {
      e <- get(id) if e.isDefined
      u <- dao.update(dbModel(p, e.get.index, Some(id)))
    } yield u.toUniqueEntity
  }

  def invalidate(id: UUID): Future[AssignmentEntry] = {
    val query = for {
      all <- dao.withSameLabworkAs(id)
      indices = reorderIndices(all.toList, id)
      deleted <- dao.invalidateSingle(id)
      _ <- dao.updateIndices(indices)
    } yield deleted.toUniqueEntity

    dao.transaction(query)
  }

  def reorderIndices(entries: List[AssignmentEntryDb], without: UUID): List[(UUID, Int)] = {
    entries
      .sortBy(_.index)
      .filterNot(_.id == without)
      .zipWithIndex
      .map(t => (t._1.id, t._2))
  }

  def getAtomic(id: UUID): Future[Option[AssignmentEntryAtom]] =
    dao.getSingle(id).map(_.map(_.asInstanceOf[AssignmentEntryAtom]))

  def getManyAtomic(ids: List[UUID]): Future[Seq[AssignmentEntryAtom]] = {
    dao.getMany(ids).map(_.map(_.asInstanceOf[AssignmentEntryAtom]))
  }

  def get(id: UUID): Future[Option[AssignmentEntry]] =
    dao.getSingle(id, atomic = false).map(_.map(_.asInstanceOf[AssignmentEntry]))

  final def dbModel(p: AssignmentEntryProtocol, index: Int, id: Option[UUID]): AssignmentEntryDb = {
    val uuid = id getOrElse UUID.randomUUID

    AssignmentEntryDb(
      p.labwork,
      index,
      p.label,
      p.types.map(t => AssignmentEntryTypeDb(uuid, t.entryType)),
      p.duration,
      id = uuid
    )
  }


}

final class AssignmentEntryServiceImpl @Inject()(
  val dao: AssignmentEntryDao,
  val ctx: ExecutionContext
) extends AssignmentEntryService
