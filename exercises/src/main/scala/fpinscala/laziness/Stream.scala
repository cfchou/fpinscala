package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) => if (n > 0) cons[A](h(), t().take(n - 1)) else empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) => if (n > 0) t().drop(n - 1) else t()
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    // `acc` is the result of foldRight over the rest
    foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else empty)
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, acc) => p(a) && acc)
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = this match {
    case Empty => empty
    case Cons(h, t) => cons(f(h()), t().map(f))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, acc) => if (f(a)) cons(a, acc) else acc)
  }

  /*
  B <: A && Stream[B] <: Stream[A]
  def foo(sa: Stream[A], a: A) {
    sa.append(a)
  }

  foo(sb, a)
  foo(sb, b)
   */
  def append[B >: A](b: => Stream[B]): Stream[B] = foldRight(b)((a, acc) => cons(a, acc))

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}