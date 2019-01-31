package utils

import base.TestBaseDefinition
import org.scalacheck.{Gen => Generator}
import org.scalatest.WordSpec
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json

final class GZIPSpec extends WordSpec with TestBaseDefinition with PropertyChecks {

  "A GZIP" should {

    "compress and decompress strings properly" in {
      val strings = for {
        c1 <- Generator.listOfN(10, Generator.numChar)
        c2 <- Generator.listOfN(10, Generator.alphaUpperChar)
        c3 <- Generator.listOfN(10, Generator.alphaLowerChar)
        c4 <- Generator.listOfN(10, Generator.alphaChar)
        c5 <- Generator.listOfN(10, Generator.alphaNumChar)
      } yield List(c1, c2, c3, c4, c5).flatten.mkString

      forAll(strings) { string =>
        (StringBasedGZIP.compress _ andThen StringBasedGZIP.decompress) (string) shouldBe string
      }
    }

    "compress and decompress empty strings properly" in {
      (StringBasedGZIP.compress _ andThen StringBasedGZIP.decompress) ("") shouldBe empty
    }

    "compress and decompress json objects properly" in {
      val json = for {
        n <- Generator.choose(0, 100)
      } yield Json.obj(
        "string" -> n.toString,
        "int" -> n,
        "bool" -> (n % 2 == 0)
      )

      forAll(json) { j =>
        (JsonBasedGZIP.compress _ andThen JsonBasedGZIP.decompress) (j) shouldBe j
      }
    }
  }
}
