package bind

import base.SesameDbSpec
import models.SesameBlacklist
import org.joda.time.DateTime
import org.w3.banana.PointedGraph

import scala.util.{Failure, Success}

class BlacklistBindingSpec extends SesameDbSpec {

  import bindings.{BlacklistDescriptor, dateTimeBinder, uuidBinder}
  import ops._

  implicit val blacklistBinder = BlacklistDescriptor.binder

  val dates = (0 until 10).map(DateTime.now.plusWeeks).toSet
  val blacklist = SesameBlacklist("blacklist", dates)

  val blacklistGraph = (
    URI(SesameBlacklist.generateUri(blacklist)).a(lwm.Blacklist)
      -- lwm.label ->- blacklist.label
      -- lwm.dates ->- blacklist.dates
      -- lwm.invalidated ->- blacklist.invalidated
      -- lwm.id ->- blacklist.id
    ).graph

  "A BlacklistBindingSpec" should {
    "return a RDF graph representation of a blacklist" in {
      val graph = blacklist.toPG.graph

      graph isIsomorphicWith blacklistGraph shouldBe true
    }

    "return a blacklist based on a RDF graph representation" in {
      val expectedBlacklist = PointedGraph[Rdf](URI(SesameBlacklist.generateUri(blacklist)), blacklistGraph).as[SesameBlacklist]

      expectedBlacklist match {
        case Success(s) =>
          s shouldEqual blacklist
        case Failure(e) =>
          fail(s"Unable to deserialise blacklist graph: $e")
      }
    }
  }
}
