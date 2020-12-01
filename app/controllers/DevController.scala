package controllers

import controllers.helper.{JsonParser, ResultOps}
import javax.inject.Inject
import play.api.mvc.{AbstractController, ControllerComponents}
import service.Webservice
import utils.date.DateTimeFormatterPattern

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class DevController @Inject()(
  cc: ControllerComponents,
  implicit val ctx: ExecutionContext,
  val ws: Webservice
) extends AbstractController(cc)
  with DateTimeFormatterPattern
  with JsonParser
  with ResultOps {

  def go() = Action.async(parse.json) { request =>
    import utils.Ops.MonadInstances.tryM
    import utils.Ops._

    val token = request.body.\("token").as[String]
    val base = "http://praktikum.gm.fh-koeln.de:9000"

    def mergeManyGets[A](request: List[Future[Try[List[A]]]]): Future[List[A]] =
      Future.sequence(request)
        .map(_.sequence.map(_.flatten))
        .flatMap(Future.fromTry)

    ???
  }
}
