package services

import models.labwork.TimetableDateEntry
import models.semester.Blacklist
import org.joda.time.{LocalDate, DateTime}
import org.w3.banana.sesame.Sesame
import store.SesameRepository
import store.bind.Bindings

trait BlacklistServiceLike {

  def applyBlacklist(entries: Set[TimetableDateEntry], localBlacklist: Set[DateTime]): Set[TimetableDateEntry]
}

class BlacklistService(private val repository: SesameRepository) extends BlacklistServiceLike {

  private val bindings = Bindings[Sesame](repository.namespace)

  override def applyBlacklist(entries: Set[TimetableDateEntry], localBlacklist: Set[DateTime]): Set[TimetableDateEntry] = {
    import bindings.BlacklistBinding._

    val globalBlacklist = repository.get[Blacklist].getOrElse(Set(Blacklist.empty)).foldLeft(Set.empty[DateTime]) {
      case (set, blacklist) => set ++ blacklist.dates
    }

    entries.filterNot(e =>
      globalBlacklist.exists(g => g.toLocalDate.isEqual(e.date)) || localBlacklist.exists(l => l.toLocalDateTime.isEqual(TimetableDateEntry.toLocalDateTime(e)))
    )
  }
}