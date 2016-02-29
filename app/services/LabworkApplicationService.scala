package services

import java.util.UUID

import models.Labwork
import models.applications.LabworkApplication
import org.openrdf.model.Value
import org.w3.banana.RDFPrefix
import store.Prefixes.LWMPrefix
import store.SesameRepository
import store.bind.Bindings
import store.sparql.select
import store.sparql.select._

import scala.util.Try

trait LabworkApplicationServiceLike {

  def applicationsFor(labwork: UUID): Try[Set[LabworkApplication]]

}

case class LabworkApplicationService(private val repository: SesameRepository) extends LabworkApplicationServiceLike {

  import repository._

  private val lwm = LWMPrefix[Rdf]
  private val rdf = RDFPrefix[Rdf]
  private val bindings = Bindings[Rdf](namespace)

  import bindings.LabworkApplicationBinding._
  import utils.Ops.MonadInstances.listM

  override def applicationsFor(labwork: UUID): Try[Set[LabworkApplication]] = {
    val laburi = Labwork.generateUri(labwork)
    val result = repository.prepareQuery {
      select("s", "timestamp") where {
        ^(v("s"), p(lwm.labwork), s(laburi)) .
          ^(v("s"), p(rdf.`type`), s(lwm.LabworkApplication)) .
          ^(v("s"), p(lwm.timestamp), v("timestamp"))
      }
    }

    result.
      select(_.get("s")).
      transform(_.fold(List.empty[Value])(identity)).
      map(_.stringValue()).
      requestAll(ids => repository.getMany[LabworkApplication](ids)).
      run
  }
}
