package bind.semester

import base.SesameDbSpec
import models.semester.Blacklist
import org.joda.time.DateTime
import org.w3.banana.PointedGraph
import org.w3.banana.sesame.Sesame
import store.bind.Bindings

import scala.util.{Failure, Success}

class BlacklistBindingSpec extends SesameDbSpec {

  val bindings = Bindings[Sesame](namespace)
  import bindings.uuidBinder
  import bindings.jodaDateTimeBinder
  import bindings.BlacklistBinding.blacklistBinder
  import ops._

  val dates = (0 until 10).map(DateTime.now.plusWeeks).toSet
  val blacklist = Blacklist("blacklist", dates, Blacklist.randomUUID)

  val blacklistGraph = (
    URI(Blacklist.generateUri(blacklist)).a(lwm.Blacklist)
      -- lwm.label ->- blacklist.label
      -- lwm.dates ->- blacklist.dates
      -- lwm.id ->- blacklist.id
    ).graph

  "A BlacklistBindingSpec" should {
    "return a RDF graph representation of a blacklist" in {
      val graph = blacklist.toPG.graph

      graph isIsomorphicWith blacklistGraph shouldBe true
    }

    "return a blacklist based on a RDF graph representation" in {
      val expectedBlacklist = PointedGraph[Rdf](URI(Blacklist.generateUri(blacklist)), blacklistGraph).as[Blacklist]

      expectedBlacklist match {
        case Success(s) =>
          s shouldEqual blacklist
        case Failure(e) =>
          fail(s"Unable to deserialise blacklist graph: $e")
      }
    }
    }
}
