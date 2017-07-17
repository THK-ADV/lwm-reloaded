package utils

/*import TypeClasses._
import Gen._
import scalaz.Monoid
import scala.util.Random._

object Genesis {

  type GenAcc[A, E, V] = (Vector[Gen[A, E, V]], List[Evaluation[E, V]])

  def byVariation[A, E, V: Monoid](pop: Vector[A], span: Int, n: Int = 10)
                                  (f: List[Evaluation[E, V]] => Boolean)
                                  (implicit
                                   e: Eval[A, E, V],
                                   z: Zero[V],
                                   m: (Mutate[A, E, V], Mutate[A, E, V]),
                                   c: (Cross[A, E, V], Cross[A, E, V]),
                                   o: Ordering[V]) = {

    lazy val size = pop.size
    attempt(lift[A, E, V](pop), span)((a, b) => (a, b))(take(n) andThen replicateWith(size)(f))
  }


  def byTaking[A, E, V: Monoid](pop: Vector[A], span: Int, n: Int = 10)
                               (implicit
                                e: Eval[A, E, V],
                                z: Zero[V],
                                m: Mutate[A, E, V],
                                c: Cross[A, E, V],
                                o: Ordering[V]) = {
    lazy val size = pop.size
    attempt(lift[A, E, V](pop), span)((a, b) => (a.elem, b))(take(n) andThen replicate(size))
  }

  def attempt[A, E, V: Monoid, C](ind: Vector[Gen[A, E, V]], times: Int)
                                 (f: (Gen[A, E, V], Int) => C) //what to return
                                 (g: GenAcc[A, E, V] => Vector[Gen[A, E, V]]) //transformation from one generation to the other
                                 (implicit
                                  e: Eval[A, E, V],
                                  zero: Zero[V],
                                  ord: Ordering[V]): C = {
    def go(vec: Vector[Gen[A, E, V]], history: List[Evaluation[E, V]], rem: Int): C = {
      val evaluated = evaluate(vec)
      if (rem == 0) f(best(evaluated), times)
      else evaluated find (_ exists zero.apply) match {
        case Some(gen) => f(gen, times - rem)
        case None => go(g((evaluated, history)), accumulate(evaluated, history), rem - 1)
      }
    }
    go(ind, List.empty, times)
  }

  def accumulate[A, E, V](v: Vector[Gen[A, E, V]], l: List[Evaluation[E, V]])(implicit ord: Ordering[V]): List[Evaluation[E, V]] = l.+:(best(v).evaluate)

  def best[A, E, V](v: Vector[Gen[A, E, V]])(implicit ord: Ordering[V]): Gen[A, E, V] = v minBy (_.evaluate.value)

  def evaluate[A, E, V: Monoid](v: Vector[Gen[A, E, V]])(implicit ef: Eval[A, E, V]): Vector[Gen[A, E, V]] = v map (_ evaluate ef.apply)

  def lift[A, E, V: Monoid](v: Vector[A]): Vector[Gen[A, E, V]] = v map withValue[A, E, V]

  def take[A, E, V](n: Int)(implicit ord: Ordering[V]): GenAcc[A, E, V] => GenAcc[A, E, V] = {
    case (v, l) => (v sortBy (_.evaluate.value) take n, l)
  }

  def replicateWith[A, E, V](times: Int)
                            (f: List[Evaluation[E, V]] => Boolean)
                            (implicit
                             mon: Monoid[V],
                             mut: (Mutate[A, E, V], Mutate[A, E, V]),
                             cross: (Cross[A, E, V], Cross[A, E, V])): GenAcc[A, E, V] => Vector[Gen[A, E, V]] = {
    case (v, l) if f(l) => replicate(times)(mon, mut._2, cross._2)((v, l))
    case (v, l) => replicate(times)(mon, mut._1, cross._1)((v, l))
  }

  def replicate[A, E, V: Monoid](times: Int)(implicit mut: Mutate[A, E, V], cross: Cross[A, E, V]): GenAcc[A, E, V] => Vector[Gen[A, E, V]] = {
    case (v, l) =>
      println(v.map(_.evaluate.value))
      (0 until times).foldLeft(v) {
      case (vec, _) =>
        val s = vec.size
        if (nextBoolean()) vec :+ vec(nextInt(s)).fold((a, e) => withValue[A, E, V](mut(a, e)))
        else {
          val (l, r) = cross(vec(nextInt(s)).fold((_, _)), vec(nextInt(s)).fold((_, _)))
          vec :+ withValue(l) :+ withValue(r)
        }
    }
  }
}

object Gen {
  def withValue[A, E, V: Monoid](v: A) = Gen(v, Evaluation.empty[E, V])
}

case class Gen[A, E, V](elem: A, evaluate: Evaluation[E, V]) {

  def evaluate(f: A => Evaluation[E, V]): Gen[A, E, V] = Gen(elem, f(elem))

  def map[B](f: A => B): Gen[B, E, V] = Gen(f(elem), evaluate)

  def exists(p: V => Boolean): Boolean = evaluate existsV p

  def fold[D](f: (A, Evaluation[E, V]) => D): D = f(elem, evaluate)
}

object TypeClasses {

  trait Eval[A, E, V] {
    def apply(a: A): Evaluation[E, V]
  }

  trait Mutate[A, E, V] {
    def apply(a: A, ev: Evaluation[E, V]): A
  }

  trait Cross[A, E, V] {
    def apply(aev1: (A, Evaluation[E, V]), aev2: (A, Evaluation[E, V])): (A, A)
  }

  trait Zero[A] {
    def apply(a: A): Boolean
  }

  object instances {
    implicit lazy val zeroInt: Zero[Int] = Zero.instance[Int](_ == 0)
  }

  object Eval {
    def instance[A, E, V](f: A => Evaluation[E, V]): Eval[A, E, V] = new Eval[A, E, V] {
      override def apply(a: A): Evaluation[E, V] = f(a)
    }
  }

  object Mutate {
    def instance[A, E, V](f: (A, Evaluation[E, V]) => A): Mutate[A, E, V] = new Mutate[A, E, V] {
      override def apply(a: A, ev: Evaluation[E, V]): A = f(a, ev)
    }
  }

  object Cross {
    def instance[A, E, V](f: ((A, Evaluation[E, V]), (A, Evaluation[E, V])) => (A, A)): Cross[A, E, V] = new Cross[A, E, V] {
      override def apply(aev1: (A, Evaluation[E, V]), aev2: (A, Evaluation[E, V])): (A, A) = f(aev1, aev2)
    }
  }

  //Law: coll find zero.apply map (_ == coll.min)
  object Zero {
    def instance[A](f: A => Boolean): Zero[A] = new Zero[A] {
      override def apply(a: A): Boolean = f(a)
    }
  }
}*/