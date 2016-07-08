package utils

import play.api.mvc.Result

object Attempt {
  def apply[T](elm: T): Attempt[T] = Continue(elm)
}

sealed trait Attempt[+T] {
  def map[U](f: T => U): Attempt[U] = flatMap(f andThen Continue.apply)

  def flatMap[U](f: T => Attempt[U]): Attempt[U] = this match {
    case Continue(a) => f(a)
    case Return(r) => Return(r)
  }

  def when[U](p: T => Boolean,
              f: T => Attempt[U])
             (fallback: => Result): Attempt[U] = this match {
    case Continue(a) if p(a) => f(a)
    case Continue(_) => Return(fallback)
    case Return(r) => Return(r)
  }

  def mapResult(f: T => Result): Result = this match {
    case Continue(x) => f(x)
    case Return(r) => r
  }
}

case class Continue[+T](i: T) extends Attempt[T]
case class Return(r: Result) extends Attempt[Nothing]

