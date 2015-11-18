package utils

import scala.annotation.tailrec
import scala.collection.GenTraversable
import scala.language.higherKinds
import scala.util.Try
import scalaz.Monad


/*
 * A number of our repository functions return nested Monads like `Try[Option[_]]`.
 * This, when used with other functions, which again return `Try` or `Option`, can lead
 * to something like `Option[Try[Option[_]]]`. It is, most of the time, quite unpleasant working
 * with such a type.
 *
 * This allows for the following morphism F[G[_]] -> G[F[_]] such that:
 * Option1[Try[Option2[A]]] -> Try[Option1.Option2[A].flatten] -> Try[Option[A]]
 */
object Ops {

  implicit val optApplicative = new Monad[Option] {
    override def bind[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa flatMap f

    override def point[A](a: => A): Option[A] = Some(a)
  }

  implicit val tryApplicative = new Monad[Try] {
    override def bind[A, B](fa: Try[A])(f: (A) => Try[B]): Try[B] = fa flatMap f

    override def point[A](a: => A): Try[A] = Try(a)
  }


  def sequence[F[+_], A](z: GenTraversable[F[A]])(implicit AF: Monad[F]): F[Vector[A]] = {
    import AF.monadSyntax._
    @tailrec
    def go(toGo: GenTraversable[F[A]], soFar: F[Vector[A]]): F[Vector[A]] = {
      if (toGo.isEmpty) soFar
      else {
        go(toGo.tail, soFar flatMap (va => toGo.head map (a => va :+ a)))
      }
    }
    go(z, point(Vector()))
  }
}

