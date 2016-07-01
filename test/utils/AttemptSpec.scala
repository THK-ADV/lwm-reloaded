package utils

import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks
import play.api.mvc.Results
import scalaz.Monad

class AttemptSpec extends WordSpec with Matchers with PropertyChecks {


  val monad: Monad[Attempt] = new Monad[Attempt] {
    override def point[A](a: => A): Attempt[A] = Continue(a)

    override def bind[A, B](fa: Attempt[A])(f: (A) => Attempt[B]): Attempt[B] = fa flatMap f
  }


  "Attempt Monad should" should {

    "satisfy the left identity law" in {
      val f = (i: Int) => {
        if (i % 40 == 0) Continue(i + 1)
        else Return(Results.InternalServerError("No"))
      }

      forAll("a") { (a: Int) =>
        (monad.point(a) flatMap f) == f(a)
      }
    }

    "satisfy the right identity law" in {
      forAll("a") { (a: Int) =>
        monad.point(a).flatMap(_a => monad.point[Int](_a)) == monad.point(a)
      }
    }

    "satisfy the associativity law" in {
      val f = (i: Int) => {
        if (i % 35 == 0) Continue(i * 2)
        else Return(Results.InternalServerError("No"))
      }

      val g = (i: Int) => {
        if (i % 40 == 0) Continue(i * 4)
        else Return(Results.InternalServerError("No"))
      }

      forAll("a") { (a: Int) =>
        monad.point(a).flatMap(f).flatMap(g) == monad.point(a).flatMap(x => f(x).flatMap(g))
      }
    }

    "satisfy the functor identity law" in {
      forAll("a") { (a: Int) =>
        monad.point(a).map(identity) == monad.point(a)
      }
    }

    "satisfy the functor function composition law" in {
      val f = (i: Int) => i + 1
      val g = (i: Int) => i * 4

      forAll("a") { (a: Int) =>
        monad.point(a).map(f).map(g) == monad.point(a).map(g compose f)
      }
    }

    "fold conditionally" in {
      val p = (i: Int) => i > 0

      forAll("a") { (a: Int) =>
        whenever(p(a)) {
          monad.point(a)
            .when(p, z => monad.point(z + 1))(
              Results.InternalServerError("No")
            ) == monad.point(a + 1)
        }
      }

      forAll("b") { (b: Int) =>
        whenever(!p(b)) {
          monad.point(b)
            .when(!p(_), z => monad.point(z + 1))(
              Results.InternalServerError("No")
            ) == Return(Results.InternalServerError("No"))
        }
      }
    }

    "unwrap itself" in {
      forAll("a") { (a: String) =>
        monad.point(a).mapResult(s => Results.Ok(s)) == Results.Ok(a)
      }
    }
  }
}
