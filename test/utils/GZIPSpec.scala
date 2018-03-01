package utils

import java.util.UUID

import base.TestBaseDefinition
import models.PostgresLabwork
import org.scalatest.WordSpec
import play.api.libs.json.Json

final class GZIPSpec extends WordSpec with TestBaseDefinition {

  "A GZIP" should {

    "compress and decompress strings properly" in {
      val simpleString = "She borrowed the book from him many years ago and hasn't yet returned it."
      val json = (0 until 100).
        map(i => PostgresLabwork(i.toString, i.toString, UUID.randomUUID, UUID.randomUUID, UUID.randomUUID)).
        map(l => Json.toJson(l)(PostgresLabwork.writes))

      (StringBasedGZIP.compress _ andThen StringBasedGZIP.decompress) (simpleString) shouldBe simpleString
      json.map(j => (JsonBasedGZIP.compress _ andThen JsonBasedGZIP.decompress) (j)) shouldBe json
    }
  }
}
