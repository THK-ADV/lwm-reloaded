package security

import play.api.mvc.{Request, WrappedRequest}

case class IdRequest[A](unwrapped: Request[A], systemId: String) extends WrappedRequest[A](unwrapped)
