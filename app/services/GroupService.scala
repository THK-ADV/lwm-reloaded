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
        sortWithPairs(nodes)
    }
  }


  def applicationsFor(labwork: UUID): Option[Vector[LabworkApplication]] = {
    val result = repository.query {
      select("id") where {
        ^(v("id"), p(lwm.labwork), o(Labwork.generateUri(labwork))).
          ^(v("id"), p(rdf.typ), s(lwm.LabworkApplication))
      }
    }

    for {
      map <- result
      values <- map.get("id")
      asStrings = values.map(_.stringValue())
      applications <- repository.getMany[LabworkApplication](asStrings).toOption
    } yield applications.toVector
  }
}
