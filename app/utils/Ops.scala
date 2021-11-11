package utils

import play.api.libs.json.{JsPath, OWrites, Writes}
import scalaz._

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.language.{higherKinds, implicitConversions}
import scala.util.{Failure, Success, Try}

object Ops {
  self =>

  object MonoidInstances {
    implicit val intM: Monoid[Int] = new Monoid[Int] {
      override def append(f1: Int, f2: => Int): Int = f1 + f2

      override def zero: Int = 0
    }
  }

  object FunctorInstances {
    implicit val setF: Functor[Set] = new Functor[Set] {
      override def map[A, B](fa: Set[A])(f: (A) => B): Set[B] = fa map f
    }
  }

  object MonadInstances {
    implicit val vecM: Monad[Vector] = new Monad[Vector] {
      override def bind[A, B](fa: Vector[A])(f: (A) => Vector[B]): Vector[B] =
        fa flatMap f

      override def point[A](a: => A): Vector[A] = Vector(a)
    }

    implicit val listM: Monad[List] = new Monad[List] {
      override def point[A](a: => A): List[A] = List(a)

      override def bind[A, B](fa: List[A])(f: (A) => List[B]): List[B] =
        fa flatMap f
    }

    implicit val optM: Monad[Option] = new Monad[Option] {
      override def bind[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] =
        fa flatMap f

      override def point[A](a: => A): Option[A] = Some(a)
    }

    implicit val tryM: Monad[Try] = new Monad[Try] {
      override def bind[A, B](fa: Try[A])(f: (A) => Try[B]): Try[B] =
        fa flatMap f

      override def point[A](a: => A): Try[A] = Try(a)
    }

    implicit val setM: Monad[Set] = new Monad[Set] {
      override def bind[A, B](fa: Set[A])(f: (A) => Set[B]): Set[B] =
        fa flatMap f

      override def point[A](a: => A): Set[A] = Set(a)
    }
  }

  object TraverseInstances {
    implicit val travT: Traverse[Try] = new Traverse[Try] {
      override def traverseImpl[G[_], A, B](
          fa: Try[A]
      )(f: (A) => G[B])(implicit ap: Applicative[G]): G[Try[B]] = {
        fa match {
          case Success(a) => ap.map(f(a))(Try(_))
          case Failure(e) => ap.point(Try(throw e))
        }
      }
    }

    implicit val travO: Traverse[Option] = new Traverse[Option] {
      override def traverseImpl[G[_], A, B](
          fa: Option[A]
      )(f: (A) => G[B])(implicit ap: Applicative[G]): G[Option[B]] = {
        fa.fold(ap.point(Option.empty[B])) { a =>
          ap.map(f(a))(Option(_))
        }
      }
    }

    implicit val travS: Traverse[Set] = new Traverse[Set] {
      override def traverseImpl[G[_], A, B](
          fa: Set[A]
      )(f: (A) => G[B])(implicit ap: Applicative[G]): G[Set[B]] = fa match {
        case set if set.nonEmpty =>
          fa.map(f).foldLeft(ap.point(Set.empty[B])) { (gs, gb) =>
            ap.apply2(gs, gb)(_ + _)
          }
        case _ => ap.point(Set.empty[B])
      }
    }
  }

  object NaturalTrasformations {
    implicit def identity[F[_]]: F ~> F = new (F ~> F) {
      override def apply[A](fa: F[A]): F[A] = fa
    }
  }

  //optimized sequence for traversables
  //We could actually use `sequenceM` for this as well, but this is specialized for scala collections and thus, faster
  def sequence[F[+_], A, M[X] <: TraversableOnce[X]](
      z: M[F[A]]
  )(implicit M: Monad[F], cbf: CanBuildFrom[M[A], A, M[A]]): F[M[A]] = {
    import M.monadSyntax._

    def go(toGo: List[F[A]], soFar: F[mutable.Builder[A, M[A]]]): F[M[A]] =
      toGo match {
        case h :: t => go(t, h flatMap (a => soFar map (_ += a)))
        case Nil    => soFar map (_.result())
      }

    go(z.toList, point(cbf()))
  }

  def sequenceM[F[_], M[_], A](
      F: F[M[A]]
  )(implicit T: Traverse[F], A: Applicative[M]): M[F[A]] = T.sequence(F)

  def flatPeek[F[_], G[_], A, B](F: F[G[A]])(f: A => F[G[B]])(implicit
      MF: Monad[F],
      MG: Monad[G],
      TF: Traverse[F],
      TG: Traverse[G]
  ): F[G[B]] = {
    MF.bind[G[A], G[B]](F) { G =>
      sequenceM(MG.bind(G)(a => sequenceM(f(a))))
    }
  }

  def peek[F[_]: Functor, G[_]: Functor, A, B](
      F: F[G[A]]
  )(f: A => B): F[G[B]] = {
    import scalaz.syntax.monad._
    F map (_ map f)
  }

  def mergePeek[F[_]: Functor, G[_]: Monad, A, B](
      F: F[G[A]]
  )(f: A => G[B]): F[G[B]] = {
    import scalaz.syntax.monad._
    F map (_ flatMap f)
  }

  def bipeek[F[_]: Applicative, G[_]: Applicative, A, B, C](
      F1: F[G[A]],
      F2: F[G[B]]
  )(f: (A, B) => C): F[G[C]] = {
    import scalaz.syntax.applicative._
    (F1 |@| F2) { (g1, g2) =>
      (g1 |@| g2)(f)
    }
  }

  def time[A](msg: String)(f: () => A): A = {
    val start = System.nanoTime()
    val res = f()
    val end = System.nanoTime()
    println(s"$msg took: ${(end - start) / 1e6} ms")
    res
  }

  implicit class TravOps[A, F[X] <: TraversableOnce[X]](Z: F[A]) {
    def foldMap[B](b: B, f: A => B)(g: (B, B) => B): B =
      Z.foldLeft(b)((b, a) => g(b, f(a)))
  }

  implicit class SeqOps[F[+_], A, M[X] <: TraversableOnce[X]](z: M[F[A]]) {
    def sequence(implicit
        M: Monad[F],
        cbf: CanBuildFrom[M[A], A, M[A]]
    ): F[M[A]] = self.sequence[F, A, M](z)
  }

  implicit class MOps[F[_], G[_], A](F: F[G[A]]) {
    def sequenceM(implicit T: Traverse[F], A: Applicative[G]) =
      self.sequenceM(F)

    def peek[B](f: A => B)(implicit F1: Functor[F], F2: Functor[G]): F[G[B]] =
      self.peek(F)(f)

    def mergePeek[B](
        f: A => G[B]
    )(implicit F1: Functor[F], M: Monad[G]): F[G[B]] = self.mergePeek(F)(f)

    def bipeek[B, C](F2: F[G[B]])(
        f: (A, B) => C
    )(implicit A1: Applicative[F], A2: Applicative[G]): F[G[C]] =
      self.bipeek(F, F2)(f)

    def flatPeek[B](f: A => F[G[B]])(implicit
        MF: Monad[F],
        MG: Monad[G],
        TF: Traverse[F],
        TG: Traverse[G]
    ): F[G[B]] = self.flatPeek(F)(f)
  }

  implicit class JsPathX(p: JsPath) {
    def writeSet[A](implicit w: Writes[A]): OWrites[Set[A]] =
      Writes.at[Set[A]](p)(Writes.set(w))

    def writeSeq[A](implicit w: Writes[A]): OWrites[Seq[A]] =
      Writes.at[Seq[A]](p)(Writes.seq(w))
  }

  implicit class FutureOps[T](val futures: List[Future[T]])(implicit
      executionContext: ExecutionContext
  ) {

    private def futureToFutureTry(f: Future[T]): Future[Try[T]] =
      f.map(s => Success(s)).recover { case t => Failure(t) }

    def asTrys: Future[List[Try[T]]] =
      Future.sequence(futures.map(futureToFutureTry))
  }

  implicit class OptionOps[A](val option: Option[A]) {
    def toTry(message: String): Try[A] = option match {
      case Some(a) => Success(a)
      case None    => Failure(new Throwable(message))
    }

    def toFuture: Future[A] = Future.fromTry(option.toTry("option was empty"))
  }

  implicit class TryOps[A](val t: Try[A]) {
    def toFuture = Future.fromTry(t)
  }

  def unwrapTrys[T](
      partialCreated: List[Try[T]]
  ): (List[T], List[Throwable]) = {
    val succeeded = partialCreated.collect { case Success(s) => s }
    val failed = partialCreated.collect { case Failure(e) => e }

    (succeeded, failed)
  }

  // This is a copy of scalas withFilter function, where you can provide a custom message for the failure case
  def when[A](f: Future[A])(p: A => Boolean)(elseMsg: () => String)(implicit
      ctx: ExecutionContext
  ): Future[A] =
    f.map(r => if (p(r)) r else throw new Throwable(elseMsg()))

  def whenNonEmpty[A](f: Future[Seq[A]])(elseMsg: () => String)(implicit
      ctx: ExecutionContext
  ): Future[Seq[A]] =
    when(f)(_.nonEmpty)(elseMsg)

  def whenDefined[A](f: Future[Option[A]], elseMsg: () => String)(implicit
      ctx: ExecutionContext
  ): Future[Option[A]] =
    when(f)(_.isDefined)(elseMsg)

  def unwrap[A](f: Future[Option[A]], elseMsg: () => String)(implicit
      ctx: ExecutionContext
  ): Future[A] =
    whenDefined(f, elseMsg).map(_.get)

  def zipOpt[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    for {
      a0 <- a
      b0 <- b
    } yield (a0, b0)
}
