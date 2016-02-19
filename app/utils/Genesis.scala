package utils

import TypeClasses._
import scalaz.{Monoid, Semigroup}
import scala.util.Random._
import Gen._

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

    def take2 = (z: GenAcc[A, E, V]) => {
      (z._1 sortBy (_.evaluate.value) take n, z._2)
    }
    lazy val size = pop.size
    (lift[A, E, V] _ andThen evalAccum(span)(take2 andThen replVar(size)(f)))(pop).elem
  }

  def measureByVariation[A, E, V: Monoid](pop: Vector[A], span: Int, n: Int = 10)
                                         (f: List[Evaluation[E, V]] => Boolean)
                                         (implicit
                                          e: Eval[A, E, V],
                                          z: Zero[V],
                                          m: (Mutate[A, E, V], Mutate[A, E, V]),
                                          c: (Cross[A, E, V], Cross[A, E, V]),
                                          o: Ordering[V]) = {

    def take2 = (z: GenAcc[A, E, V]) => {
      (z._1 sortBy (_.evaluate.value) take n, z._2)
    }
    lazy val size = pop.size
    (lift[A, E, V] _ andThen evalAccumMeasure(span)(take2 andThen replVar(size)(f)))(pop)
  }

  def byTaking[A, E, V: Monoid](pop: Vector[A], span: Int, n: Int = 10)
                               (implicit
                                e: Eval[A, E, V],
                                z: Zero[V],
                                m: Mutate[A, E, V],
                                c: Cross[A, E, V],
                                o: Ordering[V]) = {
    lazy val size = pop.size
    (lift[A, E, V] _ andThen evalEagerly(span)(take(n) andThen repl(size)))(pop).elem
  }

  def measureByTaking[A, E, V: Monoid](pop: Vector[A], span: Int, n: Int = 10)
                                      (implicit
                                       e: Eval[A, E, V],
                                       z: Zero[V],
                                       m: Mutate[A, E, V],
                                       c: Cross[A, E, V],
                                       o: Ordering[V]) = {
    lazy val size = pop.size
    (lift[A, E, V] _ andThen evalMeasure(span)(take(n) andThen repl(size)))(pop) match {
      case (gen, int) => (gen, int)
    }
  }

  def byCriteria[A, E, V: Monoid](pop: Vector[A], span: Int)
                                 (implicit
                                  p: V => V => Boolean,
                                  e: Eval[A, E, V],
                                  z: Zero[V],
                                  m: Mutate[A, E, V],
                                  c: Cross[A, E, V],
                                  o: Ordering[V]) = {
    lazy val size = pop.size
    (lift[A, E, V] _ andThen evalEagerly(span)(takeWith(p) andThen repl(size)))(pop).elem
  }

  def measureByCriteria[A, E, V: Monoid](pop: Vector[A], span: Int)
                                        (implicit
                                         p: V => V => Boolean,
                                         e: Eval[A, E, V],
                                         z: Zero[V],
                                         m: Mutate[A, E, V],
                                         c: Cross[A, E, V],
                                         o: Ordering[V]) = {
    lazy val size = pop.size
    (lift[A, E, V] _ andThen evalMeasure(span)(takeWith(p) andThen repl(size)))(pop) match {
      case (gen, int) => (gen.elem, int)
    }
  }

  def evalEagerly[A, E, V: Monoid](times: Int)
                                  (endo: Vector[Gen[A, E, V]] => Vector[Gen[A, E, V]])
                                  (implicit
                                   e: Eval[A, E, V],
                                   zero: Zero[V],
                                   ord: Ordering[V]): Vector[Gen[A, E, V]] => Gen[A, E, V] = v => {
    def go(vec: Vector[Gen[A, E, V]], rem: Int): Gen[A, E, V] = {
      val z = eval(vec)
      if(rem == 0) min(ord)(z)
      else {
        z find (_ exists zero.apply) match {
          case Some(gen) => gen
          case None => go(endo(z), rem - 1)
        }
      }
    }
    go(v, times)
  }

  def evalMeasure[A, E, V: Monoid](times: Int)
                                  (endo: Vector[Gen[A, E, V]] => Vector[Gen[A, E, V]])
                                  (implicit
                                   e: Eval[A, E, V],
                                   zero: Zero[V],
                                   ord: Ordering[V]): Vector[Gen[A, E, V]] => (Gen[A, E, V], Int) = v => {
    def go(vec: Vector[Gen[A, E, V]], rem: Int): (Gen[A, E, V], Int) = {
      val z = eval(vec)
      if(rem == 0) (min(ord)(z), times)
      else {
        z find (_ exists zero.apply) match {
          case Some(gen) => (gen, times - rem)
          case None => go(endo(z), rem - 1)
        }
      }
    }
    go(v, times)
  }

  def evalAccum[A, E, V: Monoid](times: Int)
                                (endo: GenAcc[A, E, V] => Vector[Gen[A, E, V]])
                                (implicit
                                 e: Eval[A, E, V],
                                 zero: Zero[V],
                                 ord: Ordering[V]): Vector[Gen[A, E, V]] => Gen[A, E, V] = v => {
    def go(vec: Vector[Gen[A, E, V]], rem: Int, list: List[Evaluation[E, V]]): Gen[A, E, V] = {
      val z = eval(vec)
      if(rem == 0) min(ord)(z)
      else {
        z find (_ exists zero.apply) match {
          case Some(gen) => gen
          case None =>
            go(endo(z, list), rem - 1, list.+:(min(ord)(z).evaluate))
        }
      }
    }
    go(v, times, List.empty)
  }


  def evalAccumMeasure[A, E, V: Monoid](times: Int)
                                       (endo: GenAcc[A, E, V] => Vector[Gen[A, E, V]])
                                       (implicit
                                        e: Eval[A, E, V],
                                        zero: Zero[V],
                                        ord: Ordering[V]): Vector[Gen[A, E, V]] => (Gen[A, E, V], Int) = v => {
    def go(vec: Vector[Gen[A, E, V]], rem: Int, list: List[Evaluation[E, V]]): (Gen[A, E, V], Int) = {
      val z = eval(vec)
      if(rem == 0) (min(ord)(z), times)
      else {
        z find (_ exists zero.apply) match {
          case Some(gen) => (gen, times - rem)
          case None =>
            go(endo(z, list), rem - 1, list.+:(min(ord)(z).evaluate))
        }
      }
    }
    go(v, times, List.empty)
  }

  def eval[A, E, V: Monoid](v: Vector[Gen[A, E, V]])(implicit ef: Eval[A, E, V]): Vector[Gen[A, E, V]] = v map (_ evaluate ef.apply)

  def lift[A, E, V: Monoid](v: Vector[A]): Vector[Gen[A, E, V]] = v map Gen.genE[A, E, V]

  def min[A, E, V](implicit ord: Ordering[V]): Vector[Gen[A, E, V]] => Gen[A, E, V] = _ minBy (_.evaluate.value)

  def take[A, E, V](n: Int)(implicit ord: Ordering[V]): Vector[Gen[A, E, V]] => Vector[Gen[A, E, V]] = _ sortBy(_.evaluate.value) take n

  def takeWith[A, E, V](p: V => V => Boolean)(implicit ord: Ordering[V]): Vector[Gen[A, E, V]] => Vector[Gen[A, E, V]] = v => {
    lazy val min = v.minBy(_.evaluate.value) mapErr (p, identity)
    v filter (_.existsAP(min))
  }

  def repl[A, E, V: Monoid](times: Int)
                           (implicit
                            mut: Mutate[A, E, V],
                            cross: Cross[A, E, V]): Vector[Gen[A, E, V]] => Vector[Gen[A, E, V]] =
    v => (0 until times).foldLeft(v) {
      case (vec, _) =>
        val s = vec.size
        if(nextBoolean()) vec :+ vec(nextInt(s)).fold((a, e) => genE[A, E, V](mut(a, e)))
        else {
          val (l, r) = cross(vec(nextInt(s)).fold((_, _)), vec(nextInt(s)).fold((_, _)))
          vec :+ genE(l) :+ genE(r)
        }
    }

  def replVar[A, E, V: Monoid](times: Int)(f: List[Evaluation[E, V]] => Boolean)
                              (implicit
                               mut: (Mutate[A, E, V], Mutate[A, E, V]),
                               cross: (Cross[A, E, V], Cross[A, E, V])): GenAcc[A, E, V] => Vector[Gen[A, E, V]] = {
    case (v, l) =>
      val mon: Monoid[V] = implicitly[Monoid[V]]
      if(f(l)) repl(times)(mon, mut._2, cross._2)(v)
      else repl(times)(mon, mut._1, cross._1)(v)
  }
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

  object Eval {
    def instance[A, E, V](f: A => Evaluation[E, V]): Eval[A, E, V] = new Eval[A, E, V] {
      override def apply(a: A): Evaluation[E, V] = f(a)
    }
  }
  trait Eval[-A, +E, +V] {
    def apply(a: A): Evaluation[E, V]
  }

  object Mutate {
    def instance[A, E, V](f: (A, Evaluation[E, V]) => A): Mutate[A, E, V] = new Mutate[A, E, V] {
      override def apply(a: A, ev: Evaluation[E, V]): A = f(a, ev)
    }
  }
  trait Mutate[A, E, V] {
    def apply(a: A, ev: Evaluation[E, V]): A
  }

  object Cross {
    def instance[A, E, V](f: ((A, Evaluation[E, V]), (A, Evaluation[E, V])) => (A, A)): Cross[A, E, V] = new Cross[A, E, V] {
      override def apply(aev1: (A, Evaluation[E, V]), aev2: (A, Evaluation[E, V])): (A, A) = f(aev1, aev2)
    }
  }
  trait Cross[A, E, V] {
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
