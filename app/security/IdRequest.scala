package security

import play.api.mvc.{Request, WrappedRequest}

case class IdRequest[A](private val unwrapped: Request[A], systemId: String) extends WrappedRequest[A](unwrapped)
