package store

import base.TestBaseDefinition
import org.scalatest.WordSpec
import org.w3.banana.sesame.SesameModule
import store.sparql._
import store.sparql.select._

class SDSLSpec extends WordSpec with TestBaseDefinition with SesameModule {

  def trim: String => String = s => s.replace(" ", "")

  val simpleQuery = "SELECT ?a ?b ?c WHERE { ?a <isDefined> \"concretely\" . ?b <isDefined> \"not\" . ?c <isDefined> \"notReally\" .}"
  val complexQuery = "SELECT ?a ?b ?c WHERE { ?a <isDefined> \"concretely\" . ?b <isDefined> \"not\" . FILTER(?b >= \"2\") . OPTIONAL { ?c <isDefined> \"notReally\" . } . } ORDER BY ?b"
  val askQuery = "ASK { ?a <isDefined> \"concretely\" . ?b <isDefined> \"not\" . }"

  "A SPARQL DSL" should {

    "allow simple select queries" in {
      val query =
        (select("a", "b", "c") where {
          **(v("a"), p("isDefined"), o("concretely")).
            **(v("b"), p("isDefined"), o("not")).
            **(v("c"), p("isDefined"), o("notReally"))
        }).run

      trim(query).toLowerCase shouldBe trim(simpleQuery).toLowerCase
    }


    "allow more complex select queries with additional nested and concatenated clauses" in {
      val query =
        (select("a", "b", "c") where {
          **(v("a"), p("isDefined"), o("concretely")).
            **(v("b"), p("isDefined"), o("not")).
            filter(v("b") >= o("2")).
            optional {
              **(v("c"), p("isDefined"), o("notReally"))
            }
        } orderby "b").run

      trim(query).toLowerCase shouldBe trim(complexQuery).toLowerCase
    }

    "allow ask queries" in {
      val query =
        ask {
          **(v("a"), p("isDefined"), o("concretely")).
            **(v("b"), p("isDefined"), o("not"))
        }.run

      trim(query).toLowerCase shouldBe trim(askQuery).toLowerCase
    }

    "generate correct select queries" in {
      val query =
        (select("a", "b", "c") where {
          **(v("a"), p("isDefined"), o("concretely")).
            **(v("b"), p("isDefined"), o("not")).
            filter(v("b") >= o("2")).
            optional {
              **(v("c"), p("isDefined"), o("notReally"))
            }
        } orderby "b").run

      val res = sparqlOps.parseSelect(query)

      res.isSuccess shouldBe true
    }

    "generate correct ask queries" in {
      val query =
        ask {
          **(v("a"), p("isDefined"), o("concretely")).
            **(v("b"), p("isDefined"), o("not"))
        }.run

      val res = sparqlOps.parseAsk(query)

      res.isSuccess shouldBe true
    }
  }
}
