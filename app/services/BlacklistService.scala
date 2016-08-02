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

  /**
    * Globale Blacklists sind ein Vorschlag des Tools, welche freie Tage es gibt. Locale Blacklists sind diejenigen, die
    * für das Praktikum gelten. Diejenigen globalen Blacklists, die für das Praktikum übermommen werden sollen, werden den
    * lokalen Blacklists hinzugefügt. Zudem muss dann unterschieden werden, ob
    *
    * - das DateTime nur aus einem Date besteht, wenn ja, dann soll der gesamte Tag geblacklistet werden
    * - das DateTime sowohl aus Date und Time besteht, wenn ja, dann soll an besagtem Tag jeder Termin geblacklistet werden,
    *   der enterweder um Time angefängt, oder Time innerhalb von Start und End hat, aber selbst nicht End ist
    *
    * @param entries auf die Blacklists angewendet werden sollen
    * @param localBlacklist die die Blacklists ausführt
    * @return modifizierte Einträge
    */
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