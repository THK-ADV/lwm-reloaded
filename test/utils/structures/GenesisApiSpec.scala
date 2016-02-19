package utils.structures

import utils.TypeClasses._
import utils.{Evaluation, Gen}
import utils.TypeClasses.{Eval, Zero}
import org.scalatest.{Matchers, WordSpec}
import org.scalacheck.Prop.{forAll, BooleanOperators}
import utils.Genesis._
import utils.Evaluation._
import utils.Ops.MonoidInstances._

class GenesisApiSpec extends WordSpec with Matchers {

  "A genetic algorithm" should {
    implicit val ordering = new Ordering[(Int, Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = x._1 - y._1
    }

    "apply a genesis function with possibility of early exit" in {
      implicit val eval = Eval.instance[(Int, Int), Nothing, Int](t => evaluationV(t._2))
      implicit val zero: Zero[Int] = new Zero[Int] {
        override def apply(a: Int): Boolean = a == 4
      }
      val vec = lift[(Int, Int), Nothing, Int](Vector(1, 2, 3, 4) map ((_, 0)))

      val applied = evalEagerly[(Int, Int), Nothing, Int](12)(_.map(_.map(t => (t._1, t._2 + 1)).mapErr(_ + 1, identity)))

      applied(vec).evaluate.value shouldBe 4
    }

    "take elements relative to some min value" in {
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

    "accumulate its errors with each evaluation" in {
      def ev(v: Vector[Char]) = Eval.instance[Vector[Char], Char, Int] {
        _.foldLeft(Evaluation.empty[Char, Int]) {
          case (eval, char) => if (v contains char) eval else eval mapErrWhole (_ :+ char)
        }
      }

      val accProp = forAll { (a: String, b: String) =>
        val v1 = a.toCharArray.toVector
        val v2 = b.toCharArray.toVector
        val gen = Gen.genE[Vector[Char], Char, Int](v1)
        val evaluated = eval[Vector[Char], Char, Int](Vector(gen))(intM, ev(v2))
        val diff = v1 diff v2
        evaluated.head.evaluate.err forall diff.contains
      }

      accProp.check
    }

    "apply a global accumulation function" in {
      import instances.zeroInt
      implicit val eval = Eval.instance[Int, Nothing, Int](evaluationV)
      val data = lift[Int, Nothing, Int](Vector(1, 2, 3, 4))

      val best = List(1, 2, 3)
      var parasite = List.empty[Evaluation[Nothing, Int]]

      def addOne(): GenAcc[Int, Nothing, Int] => Vector[Gen[Int, Nothing, Int]] = t => {
        parasite = t._2
        t._1 map (_ map (_ + 1))
      }
      evalAccum[Int, Nothing, Int](4)(addOne())(intM, eval, zeroInt, implicitly[Ordering[Int]])(data)

      parasite.size shouldBe best.size
      parasite map (_.value) forall best.contains shouldBe true
    }


    "variate between mutation typeclasses based on some predicate" in {
      val mut1: Mutate[Int, Nothing, Int] = Mutate.instance[Int, Nothing, Int]((i, e) => i + 1)
      val mut2: Mutate[Int, Nothing, Int] = Mutate.instance[Int, Nothing, Int]((i, e) => i + 100)
      val cross1: Cross[Int, Nothing, Int] = Cross.instance[Int, Nothing, Int] {
        case ((i1, e1), (i2, e2)) => (i1, i2)
      }
      val cross2: Cross[Int, Nothing, Int] = Cross.instance[Int, Nothing, Int] {
        case ((i1, e1), (i2, e2)) => (i1, i2)
      }

      val l1 = List(Evaluation.evaluationV(1))
      val l2 = List(Evaluation.evaluationV(1), Evaluation.evaluationV(2))
      val vec = lift[Int, Nothing, Int]((0 to 50).toVector)

      val rep1 = replVar[Int, Nothing, Int](10)(_.size % 2 == 0)(intM, (mut1, mut2), (cross1, cross2))((vec, l1))
      val rep2 = replVar[Int, Nothing, Int](10)(_.size % 2 == 0)(intM, (mut1, mut2), (cross1, cross2))((vec, l2))


      rep1 map (_.elem) forall (_ < 100) shouldBe true
      rep2 map (_.elem) exists (_ > 100) shouldBe true
    }
  }

}
