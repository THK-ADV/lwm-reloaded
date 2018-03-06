package utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import play.api.libs.json.{JsValue, Json}

import scala.io.Codec

trait GZIP[I] {
  def compress(input: I): Array[Byte]
  def decompress(bytes: Array[Byte]): I
}

object StringBasedGZIP extends GZIP[String] {
  private val codec = Codec.UTF8

  def compress(string: String): Array[Byte] = {
    val bos = new ByteArrayOutputStream(string.length)
    val gzip = new GZIPOutputStream(bos)
    gzip.write(string.getBytes(codec.name))
    gzip.close()

    val compressed = bos.toByteArray
    bos.close()
    compressed
  }

  def decompress(bytes: Array[Byte]): String = {
    val gzip = new GZIPInputStream(new ByteArrayInputStream(bytes))
    scala.io.Source.fromInputStream(gzip, codec.name).mkString
  }
}

object JsonBasedGZIP extends GZIP[JsValue] {
  override def compress(input: JsValue) = StringBasedGZIP.compress(input.toString)

  override def decompress(bytes: Array[Byte]) = Json.parse(StringBasedGZIP.decompress(bytes))
}