package utils

import utils.TypeClasses._
import scalaz.{Monoid, Semigroup}
import scala.util.Random._
import Gen._

//Optimization: The early exit strategy needs to reevaluate each individual in order to find a "perfect" one.
//Reevaluation implies that another Ω(f) steps are to be done, a subset (or all) of which are repeated in the evaluation phase.
//It might be a good idea to either keep the preexisting evaluations and omit them in the evaluation phase, or
//add some short-circuiting mechanism that can stop the evolution after the evaluation phase.
object Genesis {

  def byTaking[A, E, V: Monoid](pop: Vector[A], span: Int, n: Int = 10)
                               (implicit
                                e: EvalE[A, E, V],
                                z: Zero[V],
                                m: MutateE[A, E, V],
                                c: CrossE[A, E, V],
                                o: Ordering[V]) = {
    lazy val size = pop.size
    (lift[A, E, V] _ andThen applyNEagerly(span)(eval andThen take(n) andThen repl(size)))(pop).elem
  }

  def byCriteria[A, E, V: Monoid](pop: Vector[A], span: Int)
                                 (implicit
                                  p: V => V => Boolean,
                                  e: EvalE[A, E, V],
                                  z: Zero[V],
                                  m: MutateE[A, E, V],
                                  c: CrossE[A, E, V],
                                  o: Ordering[V]) = {
    lazy val size = pop.size
    (lift[A, E, V] _ andThen applyNEagerly(span)(eval andThen takeWith(p) andThen repl(size)))(pop).elem
  }

  def measureByTaking[A, E, V: Monoid](pop: Vector[A], span: Int, n: Int = 10)
                                      (implicit
                                       e: EvalE[A, E, V],
                                       z: Zero[V],
                                       m: MutateE[A, E, V],
                                       c: CrossE[A, E, V],
                                       o: Ordering[V]) = {
    lazy val size = pop.size
    (lift[A, E, V] _ andThen applyNMeasure(span)(eval andThen take(n) andThen repl(size)))(pop) match {
      case (gen, int) => (gen.elem, int)
    }

  }

  def measureByCriteria[A, E, V: Monoid](pop: Vector[A], span: Int)
                                        (implicit
                                         p: V => V => Boolean,
                                         e: EvalE[A, E, V],
                                         z: Zero[V],
                                         m: MutateE[A, E, V],
                                         c: CrossE[A, E, V],
                                         o: Ordering[V]) = {
    lazy val size = pop.size
    (lift[A, E, V] _ andThen applyNMeasure(span)(eval andThen takeWith(p) andThen repl(size)))(pop) match {
      case (gen, int) => (gen.elem, int)
    }
  }


  def lift[A, E, V: Monoid](v: Vector[A]): Vector[Gen[A, E, V]] = v map Gen.genE[A, E, V]

  def eval[A, E, V: Monoid](implicit ef: EvalE[A, E, V]): Vector[Gen[A, E, V]] => Vector[Gen[A, E, V]] = _ map (g => g evaluate ef.apply)

  def applyN[A, E, V](times: Int)
                     (endo: Vector[Gen[A, E, V]] => Vector[Gen[A, E, V]])
                     (implicit ord: Ordering[V]): Vector[Gen[A, E, V]] => Gen[A, E, V] = v => {
    def go(vec: Vector[Gen[A, E, V]], rem: Int): Gen[A, E, V] = {
      if (rem == 0) min(ord)(vec)
      else go(endo(vec), rem - 1)
    }
    go(v, times)
  }

  def applyNEagerly[A, E, V](times: Int)
                            (endo: Vector[Gen[A, E, V]] => Vector[Gen[A, E, V]])
                            (implicit ord: Ordering[V], zero: Zero[V], eval: EvalE[A, E, V]): Vector[Gen[A, E, V]] => Gen[A, E, V] = v => {
    def go(vec: Vector[Gen[A, E, V]], rem: Int): Gen[A, E, V] = {
      if (rem == 0) min(ord)(vec)
      else vec find (gen => eval(gen.elem).existsV(zero.apply)) match {
        case Some(gen) => gen
        case None => go(endo(vec), rem - 1)
      }
    }
    go(v, times)
  }

  def applyNMeasure[A, E, V](times: Int)
                            (endo: Vector[Gen[A, E, V]] => Vector[Gen[A, E, V]])
                            (implicit ord: Ordering[V], zero: Zero[V], eval: EvalE[A, E, V]): Vector[Gen[A, E, V]] => (Gen[A, E, V], Int) = v => {
    def go(vec: Vector[Gen[A, E, V]], rem: Int): (Gen[A, E, V], Int) = {
      if (rem == 0) (min(ord)(vec), times)
      else vec find (gen => gen evaluate eval.apply exists zero.apply) match {
        case Some(gen) => (gen, times - rem)
        case None => go(endo(vec), rem - 1)
      }
    }
    go(v, times)
  }

  def take[A, E, V](n: Int)(implicit ord: Ordering[V]): Vector[Gen[A, E, V]] => Vector[Gen[A, E, V]] = _ sortBy(_.evaluate.value) take n

  def takeWith[A, E, V](p: V => V => Boolean)(implicit ord: Ordering[V]): Vector[Gen[A, E, V]] => Vector[Gen[A, E, V]] = v => {
    lazy val min = v.minBy(_.evaluate.value) mapErr (p, identity)
    v filter (_.existsAP(min))
  }

  def repl[A, E, V: Monoid](times: Int)
                           (implicit
                            mut: MutateE[A, E, V],
                            cross: CrossE[A, E, V]): Vector[Gen[A, E, V]] => Vector[Gen[A, E, V]] =
    v => (0 until times).foldLeft(v) {
      case (vec, _) =>
        val s = vec.size
        if(nextBoolean()) vec :+ vec(nextInt(s)).fold((a, e) => genE[A, E, V](mut(a, e)))
        else {
          val (l, r) = cross(vec(nextInt(s)).fold((_, _)), vec(nextInt(s)).fold((_, _)))
          vec :+ genE(l) :+ genE(r)
        }
    }

  def min[A, E, V](implicit ord: Ordering[V]): Vector[Gen[A, E, V]] => Gen[A, E, V] = _ minBy (_.evaluate.value)

}

object Gen {
  import Evaluation._

  object ArbitraryOps {
    //Nondeterministic binary fold
    def nonDetBinFold[A, B](z: A)(t: A => B, f: A => B): B = {
      if(nextBoolean()) t(z)
      else f(z)
    }
  }

  def gen[A, E, V](v: A, e: Evaluation[E, V]) = Gen(v, e)

  def genE[A, E, V: Monoid](v: A) = Gen(v, empty[E, V])
}

case class Gen[A, E, V](elem: A, evaluate: Evaluation[E, V]) {

  def evaluate(f: A => Evaluation[E, V]): Gen[A, E, V] = Gen(elem, f(elem))

  def assimilate(f: A => Evaluation[E, V])(implicit S: Semigroup[V]): Gen[A, E, V] = Gen(elem, f(elem) :+: evaluate)

  def map[B](f: A => B): Gen[B, E, V] = Gen(f(elem), evaluate)

  def mapErr[E2, V2](f: V => V2, g: E => E2): Gen[A, E2, V2] = Gen(elem, evaluate.bimap(f, g))

  def append(e: Evaluation[E, V])(implicit S: Semigroup[V]): Gen[A, E, V] = Gen(elem, evaluate :+: e)

  def exists(p: V => Boolean): Boolean = evaluate existsV p

  def existsE(p: E => Boolean): Boolean = evaluate existsE p

  def existsAP(d: Gen[A, E, V => Boolean]): Boolean = evaluate.ap(d.evaluate).value

  def fold[D](f: (A, Evaluation[E, V]) => D): D = f(elem, evaluate)

}

object TypeClasses {

  object instances {
    implicit lazy val zeroInt: Zero[Int] = Zero.instance[Int](_ == 0)
  }

  object EvalE {
    def instance[A, E, V](f: A => Evaluation[E, V]): EvalE[A, E, V] = new EvalE[A, E, V] {
      override def apply(a: A): Evaluation[E, V] = f(a)
    }
  }
  trait EvalE[A, E, V] {
    def apply(a: A): Evaluation[E, V]
  }

  object MutateE {
    def instance[A, E, V](f: (A, Evaluation[E, V]) => A): MutateE[A, E, V] = new MutateE[A, E, V] {
      override def apply(a: A, ev: Evaluation[E, V]): A = f(a, ev)
    }
  }
  trait MutateE[A, E, V] {
    def apply(a: A, ev: Evaluation[E, V]): A
  }

  object CrossE {
    def instance[A, E, V](f: ((A, Evaluation[E, V]), (A, Evaluation[E, V])) => (A, A)): CrossE[A, E, V] = new CrossE[A, E, V] {
      override def apply(aev1: (A, Evaluation[E, V]), aev2: (A, Evaluation[E, V])): (A, A) = f(aev1, aev2)
    }
  }
  trait CrossE[A, E, V] {
    def apply(aev1: (A, Evaluation[E, V]), aev2: (A, Evaluation[E, V])): (A, A)
  }
  //Law: coll find zero.apply map (_ == coll.min)
  object Zero {
    def instance[A](f: A => Boolean): Zero[A] = new Zero[A] {
      override def apply(a: A): Boolean = f(a)
    }
  }
  trait Zero[A] {
    def apply(a: A): Boolean
  }
}
