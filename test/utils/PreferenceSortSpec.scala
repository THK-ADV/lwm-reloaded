package utils

import base.TestBaseDefinition
import org.scalatest.WordSpec
import utils.PreferenceSort._

import scala.util.Random

class PreferenceSortSpec extends WordSpec with TestBaseDefinition {
  
  def prefs[A](a: A, prfs: A*) = (a, prfs.toSet)
  
  val l = Vector(
    prefs(1, 4),
    prefs(2, 4, 5),
    prefs(3),
    prefs(4, 1, 2),
    prefs(5),
    prefs(6, 7),
    prefs(7, 9, 10),
    prefs(8, 9),
    prefs(9, 8),
    prefs(10, 7),
    prefs(11, 14),
    prefs(12, 11),
    prefs(13),
    prefs(14, 11),
    prefs(15, 2),
    prefs(16),
    prefs(17),
    prefs(18, 19, 20),
    prefs(19, 20, 18),
    prefs(20, 19, 18)
  )

  def against[A](people: Vector[Pair[A]], groups: Vector[Vector[A]]): Double = {
    val filtered = people filter (_._2.nonEmpty)
    val sat = filtered.foldLeft(0) { (ls, n) =>
      n match {
        case ((v, set)) => groups find (_ contains v) match {
          case Some(g) =>
            if(set.isEmpty) ls
            else set.foldLeft(ls) { (ns, f) =>
              if(g contains f) ns + 1
              else ns
            }
          case None => ls
        }
      }
    }
    sat.toDouble / filtered.size.toDouble
  }

  "Satisfaction percentage function" should {

    "calculate the percentage properly" in {
      val options = Vector(
        prefs(1, 2),
        prefs(2, 3),
        prefs(3, 2),
        prefs(4),
        prefs(5, 4),
        prefs(6, 7)
      )
      val groups = Vector(Vector(1, 2, 3), Vector(4, 5, 6))

      val calcPerc = against(options, groups)
      val truePerc = 4.0 / 5.0 //4 out of 5 are in a group with a friend

      truePerc shouldBe calcPerc
    }
  }

  "A group division sort" should {

    "always have a lower bound of amortized satisfaction percentage of about 90%" in {
      (1 to 1000) foreach { _ =>
        val depth = 100
        val percentage = (1 to depth).foldLeft(0.0) { (sat, _) =>
          val shuffled = Random.shuffle(l)
          val gSize = {
            val rnd = Random.nextInt(7)
            if (rnd < 5) 5
            else rnd
          }

          val sortedGrouped = sort(shuffled).grouped(gSize).toVector
          sat + against(l, sortedGrouped)
        }
        (percentage / depth) >= 0.9 shouldBe true
      }
    }
  }

}
