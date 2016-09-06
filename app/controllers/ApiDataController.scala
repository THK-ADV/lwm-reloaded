package controllers

import models.semester.Blacklist
import org.joda.time.DateTime
import org.openrdf.query.QueryLanguage
import org.w3.banana.RDFPrefix
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import store.Prefixes.LWMPrefix
import store.SesameRepository
import store.bind.Bindings

import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class ApiDataController(private val repository: SesameRepository) extends Controller {

  implicit val ns = repository.namespace

  private val bindings = Bindings[repository.Rdf](repository.namespace)

  def patchTimetableEntry = Action { implicit request =>
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
    repository.connect(_.prepareUpdate(QueryLanguage.SPARQL, update).execute())
    lookAt map (s =>  println(s map (_.getValue("v"))))

    Ok
  }

  def removeBlacklists = Action { implicit request =>
    import bindings.BlacklistDescriptor

    val result = repository.getAll[Blacklist] flatMap { blacklists =>
      repository.deleteMany[Blacklist](blacklists map Blacklist.generateUri)
    }

    result match {
      case Success(s) => Ok(Json.obj(
        "status" -> "OK",
        "message" -> s"deleted ${s.size} elements"
      ))
      case Failure(e) => InternalServerError(Json.obj(
        "status" -> "KO",
        "message" -> e.getMessage
      ))
    }
  }
}