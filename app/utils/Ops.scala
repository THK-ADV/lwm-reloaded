package utils

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.util.Try


object Ops {

  implicit val optApplicative = new Monad[Option] {
    override def apply[A](a: A): Option[A] = Option(a)

    override def flatMap[A, B](z: Option[A])(f: (A) => Option[B]): Option[B] = z flatMap f

    override def map[A, B](z: Option[A])(f: (A) => B): Option[B] = z map f
  }

  implicit val tryApplicative = new Monad[Try] {
    override def apply[A](a: A): Try[A] = Try(a)

    override def flatMap[A, B](z: Try[A])(f: (A) => Try[B]): Try[B] = z flatMap f

    override def map[A, B](z: Try[A])(f: (A) => B): Try[B] = z map f
  }

  def sequence[F[+_], A](z: Vector[F[A]])(implicit AF: Monad[F]): F[Vector[A]] = {
    import AF._
    @tailrec
    def go(toGo: Vector[F[A]], soFar: F[Vector[A]]): F[Vector[A]] = {
      if (toGo.isEmpty) soFar
      else {
        go(toGo.tail, flatMap(soFar)(va => map(toGo.head)(a => va :+ a)))
      }
    }
    go(z, apply(Vector()))
  }
}

trait Monad[F[+_]] {
  def apply[A](a: A): F[A]
  def map[A, B](z: F[A])(f: A => B): F[B]
  def flatMap[A, B](z: F[A])(f: A => F[B]): F[B]
}

