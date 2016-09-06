package controllers

import org.openrdf.query.QueryLanguage
import org.w3.banana.RDFPrefix
import play.api.mvc.{Action, Controller}
import store.Prefixes.LWMPrefix
import store.SesameRepository

import scala.util.Try

class ApiDataController(private val repository: SesameRepository) extends Controller {

  implicit val ns = repository.namespace

  def hello = Action { implicit request =>
    val lwm = LWMPrefix[repository.Rdf]
    val rdf = RDFPrefix[repository.Rdf]

    val update = s"""
      | Delete {
      | ?s <${lwm.degree}> ?v
      | } where {
      | ?s <${rdf.`type`}> <${lwm.TimetableEntry}> .
      | ?s <${lwm.degree}> ?v
      | }
    """.stripMargin

    import repository.rdfStore.sparqlEngineSyntax._
    val check =
      s"""
         |Select distinct ?v where {
         | ?s <${rdf.typ}> <${lwm.TimetableEntry}> .
         | ?s <${lwm.degree}> ?v
         |}
       """.stripMargin
    def lookAt = repository.connect(conn => repository.sparqlOps.parseSelect(check) flatMap (s => conn.executeSelect(s)))

    lookAt map (s =>  println(s map (_.getValue("v"))))
    repository.connect(c => Try(c.prepareUpdate(QueryLanguage.SPARQL, update).execute()))
    lookAt map (s =>  println(s map (_.getValue("v"))))

    Ok
  }
}