package service

import java.util.UUID

sealed trait GroupingStrategy {

  @scala.annotation.tailrec
  private def go(min: Int, max: Int, limit: Option[Int], p: Vector[UUID]): List[Vector[UUID]] = {
    (p.size % min, p.size / min) match {
      case (m, d) if m == d =>
        p.grouped(min + 1).toList
      case (m, d) if m < d =>
        val g = p.grouped(min).toList

        if (limit.contains(g.size)) {
          g
        } else {
          g.take(g.size - 1).zipAll(g.last.map(Option.apply), Vector.empty, None).map {
            case (list, Some(toAdd)) => list.+:(toAdd)
            case (list, None) => list
          }
        }
      case (m, d) if m > d =>
        go(min + 1, max, limit, p)
      case _ =>
        List.empty
    }
  }

  def apply(people: Vector[UUID]): List[Vector[UUID]] = this match {
    case CountGrouping(value) =>
      val count = value
      val min = people.size / count
      val max = min + 1

      go(min, max, Some(count), people)

    case RangeGrouping(min, max) => go(min, max, None, people)
  }
}

case class CountGrouping(value: Int) extends GroupingStrategy

case class RangeGrouping(min: Int, max: Int) extends GroupingStrategy