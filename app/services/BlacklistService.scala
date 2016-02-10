package services

import models.schedule.TimetableEntry
import models.semester.Blacklist
import org.joda.time.DateTime
import org.w3.banana.sesame.Sesame
import store.SesameRepository
import store.bind.Bindings

trait BlacklistServiceLike {

  def applyBlacklist(entries: Set[TimetableEntry], localBlacklist: Blacklist): Set[TimetableEntry]
}

class BlacklistService(private val repository: SesameRepository) extends BlacklistServiceLike {

  private val bindings = Bindings[Sesame](repository.namespace)

  override def applyBlacklist(entries: Set[TimetableEntry], localBlacklist: Blacklist): Set[TimetableEntry] = {
    import bindings.BlacklistBinding._

    val globalBlacklist = repository.get[Blacklist].getOrElse(Set(Blacklist.empty)).foldLeft(Set.empty[DateTime]) {
      case (set, blacklist) => set ++ blacklist.dates
    }
    
    entries.filterNot(e => globalBlacklist.contains(e.date) || localBlacklist.dates.contains(e.start))
  }
}