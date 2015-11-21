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

trait LabworkApplicationServiceLike {

  def applicationsFor(labwork: UUID): Option[Vector[LabworkApplication]]

}

case class LabworkApplicationService(private val repository: SesameRepository) extends LabworkApplicationServiceLike {

  import repository._

  private val lwm = LWMPrefix[Rdf]
  private val rdf = RDFPrefix[Rdf]
  private val bindings = Bindings[Rdf](namespace)

  import bindings.LabworkApplicationBinding._

  override def applicationsFor(labwork: UUID): Option[Vector[LabworkApplication]] = {
    val result = repository.query {
      select("id") where {
        ^(v("id"), p(lwm.labwork), o(labwork)) .
          ^(v("id"), p(rdf.`type`), s(lwm.LabworkApplication))
      }
    }

    for {
      map <- result
      values <- map.get("id")
      asStrings = values.map(_.stringValue())
      applications <- repository.getMany[LabworkApplication](asStrings).toOption
    } yield applications
  }
}
