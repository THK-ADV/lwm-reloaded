package utils

/*import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}
import utils.Evaluation._
import utils.Genesis._
import utils.TypeClasses._

class GenesisApiSpec extends WordSpec with Matchers with PropertyChecks {

  implicit val monInt: scalaz.Monoid[Int] = scalaz.Monoid.instance[Int](_ + _, 0)

  "A genetic algorithm" should {
    implicit val ordering = new Ordering[(Int, Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = x._1 - y._1
    }

    "apply a genesis function with possibility of early exit" in {
      implicit val eval = Eval.instance[(Int, Int), Nothing, Int](t => withValue(t._2))
      implicit val zero: Zero[Int] = new Zero[Int] {
        override def apply(a: Int): Boolean = a == 4
      }
      val vec = Genesis.lift[(Int, Int), Nothing, Int](Vector(1, 2, 3, 4) map ((_, 0)))

      val applied = attempt[(Int, Int), Nothing, Int, Gen[(Int, Int), Nothing, Int]](vec, 12)((a, _) => a) {
        case ((pop, list)) => pop.map(gen => gen.map(t => (t._1, t._2 + 1)))
      }

      applied.evaluate.value shouldBe 4
    }

    "accumulate its errors with each evaluation" in {
      def ev(v: Vector[Char]) = Eval.instance[Vector[Char], Char, Int] {
        _.foldLeft(Evaluation.empty[Char, Int]) {
          case (eval, char) => if (v contains char) eval else eval mapErrWhole (_ :+ char)
        }
      }

      forAll("a", "b"){ (a: String, b: String) =>
        val v1 = a.toCharArray.toVector
        val v2 = b.toCharArray.toVector
        val gen = Gen.withValue[Vector[Char], Char, Int](v1)
        val evaluated = evaluate[Vector[Char], Char, Int](Vector(gen))(monInt, ev(v2))
        val diff = v1 diff v2
        evaluated.head.evaluate.err forall diff.contains
      }
    }

    "apply a global accumulation function" in {
      import utils.TypeClasses.instances.zeroInt
      implicit val eval = Eval.instance[Int, Nothing, Int](withValue)
      val data = Genesis.lift[Int, Nothing, Int](Vector(1, 2, 3, 4))

      val best = List(1, 2, 3)
      var parasite = List.empty[Evaluation[Nothing, Int]]

      def addOne(t: GenAcc[Int, Nothing, Int]): Vector[Gen[Int, Nothing, Int]] = {
        parasite = t._2
        t._1 map (_ map (_ + 1))
      }

      Genesis.attempt[Int, Nothing, Int, Gen[Int, Nothing, Int]](data, 4)((a, _) => a)(addOne)
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

      val l1 = List(Evaluation.withValue(1))
      val l2 = List(Evaluation.withValue(1), Evaluation.withValue(2))
      val vec = Genesis.lift[Int, Nothing, Int]((0 to 50).toVector)

      val rep1 = Genesis.replicateWith[Int, Nothing, Int](10)(_.size % 2 == 0)(monInt, (mut1, mut2), (cross1, cross2))((vec, l1))
      val rep2 = Genesis.replicateWith[Int, Nothing, Int](10)(_.size % 2 == 0)(monInt, (mut1, mut2), (cross1, cross2))((vec, l2))


      rep1 map (_.elem) forall (_ < 100) shouldBe true
      rep2 map (_.elem) exists (_ > 100) shouldBe true
    }
  }

}

*/