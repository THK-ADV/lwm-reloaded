package services

import models.labwork.TimetableDateEntry
import models.semester.Blacklist
import org.joda.time.DateTime
import org.w3.banana.sesame.Sesame
import store.SesameRepository
import store.bind.Bindings

trait BlacklistServiceLike {

  def applyBlacklist(entries: Vector[TimetableDateEntry], localBlacklist: Set[DateTime]): Vector[TimetableDateEntry]
}

class BlacklistService(private val repository: SesameRepository) extends BlacklistServiceLike {

  private val bindings = Bindings[Sesame](repository.namespace)
  import bindings.BlacklistDescriptor

  override def applyBlacklist(entries: Vector[TimetableDateEntry], localBlacklist: Set[DateTime]): Vector[TimetableDateEntry] = {
    def checkLocal(toCheck: TimetableDateEntry, checkWith: DateTime): Boolean = {
      if (checkWith.getHourOfDay == 0)
        toCheck.date.isEqual(checkWith.toLocalDate)
      else
        toCheck.date.toDateTime(toCheck.start).isEqual(checkWith)
    }

    val globalBlacklist = repository.getAll[Blacklist].getOrElse(Set(Blacklist.empty)).foldLeft(Set.empty[DateTime]) {
      case (set, blacklist) => set ++ blacklist.dates
    }

    entries.filterNot(e => globalBlacklist.exists(g => g.toLocalDate.isEqual(e.date)) || localBlacklist.exists(l => checkLocal(e, l)))
  }
}