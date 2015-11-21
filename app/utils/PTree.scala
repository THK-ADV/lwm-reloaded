package utils

import scala.collection.GenTraversable
import scala.util.Random
import PTree._

object PTree {

  type Pair[A] = (A, List[A])

  def nodef[A](a: => A, l: List[A]) = PNode[A](a, l, leaf, leaf)

  def nodec[A](a: => A, l: List[A], left: PTree[A], right: PTree[A]) = PNode[A](a, l, left, right)

  def arrange[A](forest: TraversableOnce[PTree[A]]): PTree[A] = forest reduce (_ ++ _)

  def leaf[A]: PLeaf[A] = PLeaf()

  def node[A](a: => A) = PNode[A](a, List.empty[A], leaf, leaf)

  def nodef[A](a: => A, f: A*) = PNode[A](a, f.toList, leaf, leaf)

  def sort[A](forest: TraversableOnce[PTree[A]]): Vector[A] = {
    @annotation.tailrec
    def go(pri: PTree[A], post: Vector[A], cache: Vector[A]): Vector[A] = pri match {
      case node@PNode(v, f, _, _) if f.isEmpty => go(node.root._2, post, cache :+ node.value)
      case _ => pri.root match {
        case (Some(v), tree) => go(tree, post :+ v, cache)
        case (None, _) => post ++ cache
      }
    }
    go(arrange(forest), Vector(), Vector())
  }

  def sortWithPairs[A](pairs: TraversableOnce[Pair[A]]): Vector[A] = sort[A](pairs map (pair => nodef(pair._1, pair._2)))

}

sealed abstract class PTree[A] {

  //into right
  def #->(tree: PTree[A]): PTree[A] = this match {
    case PNode(v, f, left, right) => PNode(v, f, left, right ++ tree)
    case PLeaf() => tree
  }

  //into left
  def <-#(tree: PTree[A]): PTree[A] = this match {
    case PNode(v, f, left, right) => PNode(v, f, left ++ tree, right)
    case PLeaf() => tree
  }

  //as right
  def -->(tree: PTree[A]): PTree[A] = this match {
    case PNode(v, f, left, PLeaf()) => PNode(v, f, left, tree)
    case PNode(v, f, left, right) => PNode(v, f, left, tree) ++ right
    case PLeaf() => tree
  }

  //as left
  def <--(tree: PTree[A]): PTree[A] = this match {
    case PNode(v, f, PLeaf(), right) => PNode(v, f, tree, right)
    case PNode(v, f, left, right) => PNode(v, f, tree, right) ++ left
    case PLeaf() => tree
  }

  //insert
  def ++(tree: PTree[A]): PTree[A] = (this, tree) match {
    case (PNode(v, f, _, PLeaf()), node@PNode(_, _, _, _)) => this --> node
    case (PNode(v, f, PLeaf(), _), node@PNode(_, _, _, _)) => this <-- node
    case (PNode(v, f, left@PNode(_, lf, _, _), right@PNode(_, rf, _, _)), node@PNode(v2, _, _, _)) if f contains v2 =>
      (lf contains v2, rf contains v2) match {
        case (true, false) => this --> node
        case (false, true) => this <-- node
        case _ =>
          distribute(node)(
            newLeft => this <-- node,
            newRight => this --> node
          )
      }
    case (PNode(v, f, left@PNode(_, fl, _, _), right@PNode(_, fr, _, _)), node@PNode(v2, _, _, _)) if fl contains v2 => this <-# node
    case (PNode(v, f, left@PNode(_, fl, _, _), right@PNode(_, fr, _, _)), node@PNode(v2, _, _, _)) if fr contains v2 => this #-> node
    case (PLeaf(), _) => tree
    case _ =>
      distribute(tree)(
        intoLeft => this <-# intoLeft,
        intoRight => this #-> intoRight
      )
  }

  def root: (Option[A], PTree[A]) = this match {
    case PNode(v, f, PLeaf(), PLeaf()) => (Some(v), PLeaf())
    case PNode(v, f, left, PLeaf()) => (Some(v), left)
    case PNode(v, f, PLeaf(), right) => (Some(v), right)
    case PNode(v, f, left@PNode(lv, fl, _, _), right@PNode(rv, fr, _, _)) =>
      (fl contains v, fr contains v) match {
        case (true, false) => (Some(v), left -><- right)
        case (false, true) => (Some(v), right -><- left)
        case (true, true) => (Some(v), (left |+ rv) -><- (right |+ lv))
        case _ =>
          val tree = left.distribute(right)(intoLeft => left -><- right, intoRight => right -><- left)
          (Some(v), tree)
      }

    case PLeaf() => (None, PLeaf())
  }

  //merge
  def -><-(tree: PTree[A]): PTree[A] = tree match {
    case PLeaf() => tree
    case PNode(v, f, PLeaf(), PLeaf()) => this ++ nodef(v, f)
    case PNode(v, f, PLeaf(), right) => (this ++ nodef(v, f)) -><- right
    case PNode(v, f, left, PLeaf()) => (this ++ nodef(v, f)) -><- left
    case PNode(v, f, left, right) => ((this ++ nodef(v, f)) -><- left) -><- right
  }

  def map[B](f: A => B): PTree[B] = this match {
    case PLeaf() => PLeaf()
    case PNode(v, fz, left, right) => nodec(f(v), fz map f, left map f, right map f)
  }

  def distribute(tree: PTree[A])(left: PTree[A] => PTree[A], right: PTree[A] => PTree[A]) = this match {
    case PLeaf() => tree
    case PNode(_, _, PLeaf(), _) => left(tree)
    case PNode(_, _, _, PLeaf()) => right(tree)
    case _ =>
      if (Random.nextBoolean()) left(tree)
      else right(tree)
  }

  //with friend
  def |+(nf: A): PTree[A] = |++(List(nf))

  //with friends
  def |++(nf: List[A]): PTree[A] = this match {
    case PNode(v, f, left, right) => nodec(v, f ++ nf, left, right)
    case PLeaf() => PLeaf()
  }

  def size: Int = this match {
    case PNode(_, _, left, right) => 1 + left.size + right.size
    case PLeaf() => 0
  }

}

case class PNode[A](value: A,
                    friends: List[A],
                    left: PTree[A],
                    right: PTree[A]) extends PTree[A]

case class PLeaf[A]() extends PTree[A]
