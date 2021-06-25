package base

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

trait TestBaseDefinition
    extends BeforeAndAfterAll
    with BeforeAndAfterEach
    with Matchers
    with ScalaFutures
    with OptionValues
    with EitherValues
    with TryValues {
  this: Suite =>
}
