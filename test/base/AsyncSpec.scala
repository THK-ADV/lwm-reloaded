package base

import org.scalatest.time.{Seconds, Span}

import scala.concurrent.Future

trait AsyncSpec { self: TestBaseDefinition =>
  final def async[R](future: Future[R])(assert: R => Unit): Unit = whenReady(future, timeout(Span(5, Seconds)))(assert)
}
