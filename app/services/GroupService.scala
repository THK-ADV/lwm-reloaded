package services

import java.util.UUID

import models.applications.LabworkApplication
import store.Prefixes.LWMPrefix
import store.SesameRepository
import store.bind.Bindings

trait GroupServiceLike {

  def participantsFor(labwork: UUID): List[UUID]
}

class GroupService(repository: SesameRepository) extends GroupServiceLike {
  import repository._

  private val lwm = LWMPrefix[Rdf]
  private val bindings = Bindings[Rdf](namespace)

  override def participantsFor(labwork: UUID): List[UUID] = {
    val a = repository.get[LabworkApplication].get.filter(l => l.labwork == labwork)
    
    ???
  }
}
