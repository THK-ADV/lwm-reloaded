package utils.structures

import utils.{Evaluation, Gen}
import utils.TypeClasses.{EvalE, Zero}
import org.scalatest.{Matchers, WordSpec}
import org.scalacheck.Prop.{forAll, BooleanOperators}
import utils.Genesis._
import utils.Evaluation._

class GenesisApiSpec extends WordSpec with Matchers {

  implicit val monInt: scalaz.Monoid[Int] = scalaz.Monoid.instance[Int](_ + _, 0)

  "A genetic algorithm" should {
    implicit val ordering = new Ordering[(Int, Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = x._1 - y._1
    }

    "Apply a genesis function a given amount of times" in {
      val vec = lift[Int, Nothing, Int](Vector(1, 2, 3, 4))

      val applied = applyN[Int, Nothing, Int](12)(_.map(_.mapErr(_ + 1, identity)))

      applied(vec).evaluate.value shouldBe 12
    }

    "Apply a genesis function with possibility of early exit" in {
      implicit val eval = EvalE.instance[(Int, Int), Nothing, Int](t => evaluationV(t._2))
      implicit val zero: Zero[Int] = new Zero[Int] {
        override def apply(a: Int): Boolean = a == 4
      }
      val vec = lift[(Int, Int), Nothing, Int](Vector(1, 2, 3, 4) map ((_, 0)))

      val applied = applyNEagerly[(Int, Int), Nothing, Int](12)(_.map(_.map(t => (t._1, t._2 + 1)).mapErr(_ + 1, identity)))

      applied(vec).evaluate.value shouldBe 4
    }

    "Take elements relative to some min value" in {
      val prop = forAll { (l: List[Int]) =>
        l.nonEmpty ==> {
          val v = l.toVector
          val lifted = lift[Int, Nothing, Int](v) map (_ assimilate evaluationV)
          val index = util.Random.nextInt(v.size)
          val p = (min: Int) => (cur: Int) => cur <= min + v(index)
          val take = takeWith[Int, Nothing, Int](p)
          val taken = take(lifted) map (_.elem)
          val min = v.min
          val pr = p(min)
          taken.sorted == v.filter(pr).sorted
        }
      }

      prop.check
    }

    "Accumulate its errors with each evaluation" in {
      def ev(v: Vector[Char]) = EvalE.instance[Vector[Char], Char, Int] {
        _.foldLeft(Evaluation.empty[Char, Int]) {
          case (eval, char) => if (v contains char) eval else eval mapErrWhole (_ :+ char)
        }
      }

      val accProp = forAll { (a: String, b: String) =>
        val v1 = a.toCharArray.toVector
        val v2 = b.toCharArray.toVector
        val gen = Gen.genE[Vector[Char], Char, Int](v1)
        val evaluated = eval[Vector[Char], Char, Int](monInt, ev(v2))(Vector(gen))
        val diff = v1 diff v2
        evaluated.head.evaluate.err forall diff.contains
      }

      accProp.check
    }
  }

}
