package services

import java.util.UUID

import models.Labwork
import models.applications.LabworkApplication
import org.w3.banana.RDFPrefix
import store.Prefixes.LWMPrefix
import store.SesameRepository
import store.bind.Bindings
import store.sparql.select
import store.sparql.select._
import utils.PTree._

trait GroupServiceLike {

  def participantsFor(labwork: UUID): Option[Vector[UUID]]
}

class GroupService(repository: SesameRepository) extends GroupServiceLike {

  import repository._

  private val lwm = LWMPrefix[Rdf]
  private val rdf = RDFPrefix[Rdf]
  private val bindings = Bindings[Rdf](namespace)

  import bindings.LabworkApplicationBinding._

  override def participantsFor(labwork: UUID): Option[Vector[UUID]] = {
    applicationsFor(labwork) map { v =>
      val nodes = v map (app => (app.applicant, app.friends.toList))
        sort(nodes)
    }
  }


  def applicationsFor(labwork: UUID): Option[Vector[LabworkApplication]] = {
    import utils.Ops._

    val result = repository.query {
      select("id") where {
        ^(v("id"), p(lwm.labwork), o(Labwork.generateUri(labwork))).
          ^(v("id"), p(rdf.typ), s(lwm.LabworkApplication))
      }
    }.flatMap(_.get("id"))
    
    sequence {
      for {
        values <- result
        all <- values map (v => repository.get[LabworkApplication](v.stringValue()).toOption.flatten)
      } yield all
    }
  }
}
