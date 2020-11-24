package controllers

import controllers.helper.{JsonParser, ResultOps}
import javax.inject.Inject
import play.api.mvc.{AbstractController, ControllerComponents}
import utils.date.DateTimeFormatterPattern

import scala.concurrent.ExecutionContext

class DevController @Inject()(cc: ControllerComponents, implicit val ctx: ExecutionContext)
  extends AbstractController(cc)
    with DateTimeFormatterPattern
    with JsonParser
    with ResultOps
