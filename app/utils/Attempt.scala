package utils

import play.api.mvc.Result

object Attempt {
  def apply[X](elm: X): Attempt[X] = Continue(elm)
}

sealed trait Attempt[+X] {
  def map[Y](f: X => Y): Attempt[Y] = flatMap(f andThen Continue.apply)

  def flatMap[Y](f: X => Attempt[Y]): Attempt[Y] = this match {
    case Continue(a) => f(a)
    case Return(r) => Return(r)
  }


  def when[Y](p: X => Boolean,
              f: X => Attempt[Y])
             (fallback: => Result): Attempt[Y] = this match {
    case Continue(a) if p(a) => f(a)
    case Continue(_) => Return(fallback)
    case Return(r) => Return(r)
  }

  def mapResult(f: X => Result): Result = this match {
    case Continue(x) => f(x)
    case Return(r) => r
  }
}

case class Continue[+X](i: X) extends Attempt[X]
case class Return(r: Result) extends Attempt[Nothing]

