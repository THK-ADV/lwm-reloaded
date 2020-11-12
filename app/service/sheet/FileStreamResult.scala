package service.sheet

import controllers.helper.ResultOps
import org.apache.commons.io.output.ByteArrayOutputStream
import play.api.mvc.{BaseController, Result}

import scala.util.Try

trait FileStreamResult {
  self: BaseController with ResultOps =>

  def toResult(stream: Try[ByteArrayOutputStream]): Result =
    stream.fold(badRequest, s => Ok(s.toByteArray).as("application/vndd.ms-excel"))
}
