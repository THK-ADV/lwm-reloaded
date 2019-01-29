package security

import models.Authority
import play.api.mvc.{Request, WrappedRequest}

case class AuthRequest[A](private val unwrapped: Request[A], authorities: Seq[Authority]) extends WrappedRequest[A](unwrapped)
