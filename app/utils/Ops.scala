package utils

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
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
  self =>

  implicit val optApplicative = new Monad[Option] {
    override def bind[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa flatMap f

    override def point[A](a: => A): Option[A] = Some(a)
  }

  implicit val tryApplicative = new Monad[Try] {
    override def bind[A, B](fa: Try[A])(f: (A) => Try[B]): Try[B] = fa flatMap f

    override def point[A](a: => A): Try[A] = Try(a)
  }

  def sequence[F[+_], A, M[X] <: TraversableOnce[X]](z: M[F[A]])(implicit M: Monad[F], cbf: CanBuildFrom[M[A], A, M[A]]): F[M[A]] = {
    import M.monadSyntax._

    def go(toGo: List[F[A]], soFar: F[mutable.Builder[A, M[A]]]): F[M[A]] = toGo match {
      case h :: t => go(t, h flatMap (a => soFar map (_ += a)))
      case Nil => soFar map (_.result())
    }

    go(z.toList, point(cbf()))
  }

  implicit class SeqOps[F[+_], A, M[X] <: TraversableOnce[X]](z: M[F[A]]) {
    def sequence(implicit M: Monad[F], cbf: CanBuildFrom[M[A], A, M[A]]): F[M[A]] = self.sequence[F, A, M](z)
  }
}

