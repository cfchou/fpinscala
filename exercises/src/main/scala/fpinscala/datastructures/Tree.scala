package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def fold1[A, B](t: Tree[A])(f: A => B)(g: (Tree[A], Tree[A]) => B): B = {
    t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(l, r)
    }
  }

  def fold2[A, B](t: Tree[A])(f: Leaf[A] => B)(g: Branch[A] => B): B = {
    t match {
      case l:Leaf[A] => f(l)
      case b:Branch[A] => g(b)
    }
  }

  def sizeViaFold[A](t: Tree[A]): Int = {
    //fold1(t)(_ => 1)((l, r) => 1 + sizeViaFold(l) + sizeViaFold(r))
    //fold2(t)(_ => 1)({case Branch(l, r) => 1 + sizeViaFold(l) + sizeViaFold(r)})
    fold(t)(_ => 1)((foldedL, foldedR) => 1 + foldedL + foldedR)
  }

  def maximumViaFold(t: Tree[Int]): Int = {
    //fold1(t)(identity)((l, r) => maximumViaFold(l) max maximumViaFold(r))
    /*
    fold2(t)({
      case Leaf(v) => v
    })({
      case Branch(l, r) => maximumViaFold(l) max maximumViaFold(r)
    })
    */
    fold(t)(identity)((foldedL, foldedR) => foldedL max foldedR)
  }

  def depthViaFold[A](t: Tree[A]): Int = {
    /*
    fold2(t)(_ => 1)({
      case Branch(l, r) => 1 + (depthViaFold(l) max depthViaFold(r))})
      */
    fold(t)(_ => 1)((foldedL, foldedR) => foldedL max foldedR)
  }

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    /*
    fold2(t)({
      case Leaf(v) =>
        Leaf(f(v)): Tree[B]
    })({
      case Branch(l, r) =>
        Branch(mapViaFold(l)(f), mapViaFold(r)(f)): Tree[B]
    })
    */
    fold[A, Tree[B]](t)(v => Leaf(f(v)))((foldedL, foldedR) => Branch(foldedL, foldedR))
  }
}