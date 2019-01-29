package controllers.helper

import services.{CountGrouping, GroupingStrategy, RangeGrouping}

import scala.collection.Map
import scala.util.{Failure, Try}

trait GroupingStrategyAttributeFilter { self: AttributeFilter =>

  protected lazy val countAttribute = "count"
  protected lazy val minAttribute = "min"
  protected lazy val maxAttribute = "max"

  protected final def extractGroupingStrategy(queryString: Map[String, Seq[String]]): Try[GroupingStrategy] = {
    val v = valueOf(queryString) _

    (v(countAttribute), v(minAttribute), v(maxAttribute)) match {
      case (Some(count), None, None) =>
        for {
          c <- Try(count.toInt) if c > 0
        } yield CountGrouping(c)
      case (None, Some(min), Some(max)) =>
        for {
          a <- Try(min.toInt)
          b <- Try(max.toInt) if a < b
        } yield RangeGrouping(a, b)
      case _ =>
        Failure(new Exception(s"grouping strategy should be either $countAttribute or $minAttribute and $maxAttribute"))
    }
  }
}
