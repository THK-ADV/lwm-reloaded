package utils.structures

import base.TestBaseDefinition
import org.scalatest.WordSpec
import utils.PTree._    
import utils.{PTree, PLeaf, PNode}  

import scala.util.Random

class PTreeSpec extends WordSpec with TestBaseDefinition {

  val l = Vector(
    nodef(1, 4),
    nodef(2, 4, 5),
    node(3),
    nodef(4, 1, 2),
    nodef(5),
    nodef(6, 7),
    nodef(7, 9, 10),
    nodef(8, 9),
    nodef(9, 8),
    nodef(10, 7),
    nodef(11, 14),
    nodef(12, 11),
    node(13),
    nodef(14, 11),
    nodef(15, 2),
    node(16),
    node(17),
    nodef(18, 19, 20),
    nodef(19, 20, 18),
    nodef(20, 19, 18)
  )

  def against[A](people: Vector[PTree[A]], groups: Vector[Vector[A]]): Double = {
    val filtered = people.filter {
      case PNode(_, f, _, _) => f.nonEmpty
      case PLeaf() => false
    }
    val sat = filtered.foldLeft(0) { (ls, n) =>
      n match {
        case PNode(v, f, _, _) =>
          groups.find(_.contains(v)) match {
            case Some(g) =>
              if (f.isEmpty) ls
              else f.foldLeft(ls) { (ns, f) =>
                if (g.contains(f)) ns + 1
                else ns
              }
            case None => ls
          }
        case PLeaf() => ls
      }
    }
    sat.toDouble / filtered.size.toDouble
  }

  "Satisfaction percentage function" should {

    "calculate the percentage properly" in {
      val options = Vector(
        nodef(1, 2),
        nodef(2, 3),
        nodef(3, 2),
        node(4),
        nodef(5, 4),
        nodef(6, 7)
      )
      val groups = Vector(Vector(1, 2, 3), Vector(4, 5, 6))

      val calcPerc = against(options, groups)
      val truePerc = 4.0 / 5.0 //4 out of 5 are in a group with a friend

      truePerc shouldBe calcPerc
    }
  }

  "A group division tree" should {

    "always have a lower bound of amortized satisfaction percentage of about 86%" in {
      (1 to 10) foreach { _ =>
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
        (percentage / depth) >= 0.86 shouldBe true
      }
    }

    "should not swallow values" in {
      @annotation.tailrec
      def go[A](tree: PTree[A], n: Int): Int = tree.root match {
        case (v, tail@PNode(_, _, _, _)) =>
          go(tail, n + 1)
        case (_, PLeaf()) => n
      }
      (1 to 1000) foreach { _ =>
        val arranged = arrange(l)
        val size = go(arranged, 1)
        size shouldBe l.size
      }
    }
  }

}
