package dao

import java.util.UUID

import scala.concurrent.ExecutionContext

object ScheduleCombinatorDao {

  def invalidate(labwork: UUID)(scheduleEntryDao: ScheduleEntryDao, groupDao: GroupDao)(implicit ctx: ExecutionContext) = {
    for {
      dg <- groupDao.invalidateByLabwork(labwork)
      ds <- scheduleEntryDao.invalidateByLabwork(labwork)
    } yield (dg.map(_.toUniqueEntity), ds.map(_.toUniqueEntity))
  }
}
