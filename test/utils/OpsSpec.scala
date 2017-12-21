package utils

import base.TestBaseDefinition
import org.scalatest.prop.PropertyChecks
import scala.util.Try
import org.scalatest.WordSpec

class OpsSpec extends WordSpec with TestBaseDefinition with PropertyChecks {

  "Ops" should {

    "Sequence arbitrary collections given a proper type-classes" in {
      import utils.Ops.MonadInstances._
      import utils.Ops._

      forAll { (l: List[Int]) =>
        val m = l map Option.apply
        m.sequence shouldBe Option(l)
      }
    }

    "`Peek` through one layer of nesting and apply a mapping" in {
      import utils.Ops.MonadInstances._
      import utils.Ops._

      forAll { (o: Option[Int]) =>
        val nested1 = List(o)
        val nested2 = Try(o)

        nested1 peek (_.toString) shouldBe List(o map (_.toString))
        nested2 peek (_.toString) shouldBe Try(o map (_.toString))
      }
    }

    "Monadically `Peek` trough one layer of nesting and flatten the inner layer" in {
      import utils.Ops.MonadInstances._
      import utils.Ops._

      forAll { (o1: Option[Int], o2: Option[String]) =>
        val nested1 = List(o1)
        val nested2 = Try(o1)

        nested1 mergePeek (i => o2 map ((_, i))) shouldEqual List(o2 flatMap (s => o1 map (i => (s, i))))
        nested2 mergePeek (i => o2 map ((_, i))) shouldEqual Try(o2 flatMap (s => o1 map (i => (s, i))))
      }
    }

    "Binarily `Peek` through two separate layers of nesting, applying a mapping to the union of those layers" in {
      import utils.Ops.MonadInstances._
      import utils.Ops._

      forAll { (o1: Option[Int], o2: Option[String]) =>
        val nested1 = List(o1)
        val nested2 = Try(o1)

        nested1.bipeek(List(o2))((_, _)) shouldEqual List(o2 flatMap (s => o1 map (i => (i, s))))
        nested2.bipeek(Try(o2))((_, _)) shouldEqual Try(o2 flatMap (s => o1 map (i => (i, s))))
      }
    }
  }


}
