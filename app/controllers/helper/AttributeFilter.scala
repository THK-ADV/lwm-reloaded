package controllers.helper

import java.sql.Timestamp

import scala.collection.Map
import scala.util.Try

trait AttributeFilter {

  protected lazy val atomicAttribute = "atomic"
  protected lazy val validAttribute = "valid"
  protected lazy val lastModifiedAttribute = "lastModified"

  protected case class DefaultAttributes(atomic: Boolean = true, valid: Boolean = true, lastModified: Option[String] = None)

  protected type QueryString = Map[String, Seq[String]]

  final protected def extractAttributes(queryString: QueryString, defaultAtomic: Boolean = true): (QueryString, DefaultAttributes) = {
    def extractBool(seq: Seq[String], fallback: Boolean): Boolean = {
      seq.headOption.flatMap(s => Try(s.toBoolean).toOption).fold(fallback)(_ == true)
    }

    def extractTimestamp(seq: Seq[String]): Option[String] = {
      seq.headOption.flatMap(s => Try(s.toLong).flatMap(l => Try(new Timestamp(l))).toOption).map(_.getTime.toString)
    }

    var atomic = defaultAtomic
    var valid = true
    var lastModified: Option[String] = None

    val remaining = List(atomicAttribute, validAttribute, lastModifiedAttribute).foldLeft(queryString) {
      case (q, at) if at == atomicAttribute =>
        q.find(_._1 == at).fold(q) { seq =>
          atomic = extractBool(seq._2, fallback = false)
          q - at
        }
      case (q, inv) if inv == validAttribute =>
        q.find(_._1 == inv).fold(q) { seq =>
          valid = extractBool(seq._2, fallback = true)
          q - inv
        }
      case (q, mod) if mod == lastModifiedAttribute =>
        q.find(_._1 == mod).fold(q) { seq =>
          lastModified = extractTimestamp(seq._2)
          q - mod
        }
      case (q, _) => q
    }

    (remaining, DefaultAttributes(atomic, valid, lastModified))
  }

  protected final def intOf(queryString: Map[String, Seq[String]])(attribute: String): Option[Int] = {
    valueOf(queryString)(attribute).flatMap(s => Try(s.toInt).toOption)
  }

  protected final def boolOf(queryString: Map[String, Seq[String]])(attribute: String): Option[Boolean] = {
    valueOf(queryString)(attribute).flatMap(s => Try(s.toBoolean).toOption)
  }

  protected final def valueOf(queryString: Map[String, Seq[String]])(attribute: String): Option[String] = {
    queryString.get(attribute).flatMap(_.headOption)
  }
}