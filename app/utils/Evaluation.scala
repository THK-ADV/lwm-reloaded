package utils

import scala.language.reflectiveCalls
import scalaz.{Applicative, Semigroup, Monoid}
import scalaz.syntax.monoid._
import scalaz.std.AllInstances.listMonoid

object Evaluation {
  def lift2[E, V](v: V, e: List[E]): Evaluation[E, V] = Evaluation(e, v)
  def withValue[E, V](v: V): Evaluation[E, V] = Evaluation(List.empty[E], v)
  def withError[E, V: Monoid](e: List[E]): Evaluation[E, V] = Evaluation(e, mzero[V])
  def empty[E, V: Monoid]: Evaluation[E, V] = Evaluation(List.empty[E], mzero[V])
}

sealed case class Evaluation[+E, +V](err: List[E], value: V) {
  import Evaluation._

  def add[E2 >: E](e: E2): Evaluation[E2, V] = mapErrWhole(_.+:(e))
  def ap[E2 >: E, V2](E: Evaluation[E2, V => V2]): Evaluation[E2, V2] = E map (_ (value)) mapErrWhole (e => listMonoid.append(e, err))
  def bimap[E2, V2](f: V => V2, g: E => E2): Evaluation[E2, V2] = withValue(f(value)) mapErr g
  def map[B](f: V => B): Evaluation[E, B] = lift2(f(value), err)
  def mapErr[E2](f: E => E2): Evaluation[E2, V] = lift2(value, err map f)
  def mapErrWhole[E2](f: List[E] => List[E2]): Evaluation[E2, V] = lift2(value, f(err))
  def fold[B](f: (List[E], V) => B): B = f(err, value)
  def append[E2 >: E, V2 >: V](E: Evaluation[E2, V2])(implicit S: Semigroup[V2]): Evaluation[E2, V2] = {
    E map (v => S.append(v, value)) mapErrWhole (e => listMonoid.append(e, err))
  }
  def :+:[E2 >: E, V2 >: V](E: Evaluation[E2, V2])(implicit S: Semigroup[V2]): Evaluation[E2, V2] = append(E) //alias for append
  def foreach[U](f: V => U): Unit = f(value)
  def existsV(p: V => Boolean): Boolean = p(value)
  def existsE(p: E => Boolean): Boolean = err exists p
}