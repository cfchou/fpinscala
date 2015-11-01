package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def toList(): List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  def toList2(): List[A] = {
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h()::acc)
      case Empty => Nil
    }
    go(this, Nil)
  }

  // The arrow `=>` in front of the argument type `B` means that the function
  // `f` takes its second argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
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
  def append[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)((a, acc) => cons(a, acc))

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    // def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Empty, _) => None
      case (Cons(h, t), i) if i > 0 => Some((h(), (t(), i - 1)))
      case _ => None
    }
  }


  /* NOTE
  `f` in `unfold` should pass the state back to `unfold` instead of managing
  state itself internally(like calling takeViaUnfold2 recursively)

  // WRONG example
  def takeViaUnfold2(n: Int): Stream[A] = {
    unfold(this) {
      case Empty => None
      case Cons(h, t) =>
        if (n > 0) Some((h(), t().takeViaUnfold2(n - 1)))
        else None
    }
  }
   */

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Empty => None
      case Cons(h, t) => {
        val v = h()
        if (p(v)) Some((v, t())) else None
      }
    }
  }

  def zipWith[B,C](s: Stream[B])(f: (A,B) => C): Stream[C] = {
    unfold(this, s) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  }

  def zip[B](s: Stream[B]): Stream[(A, B)] = zipWith(s)((_, _))

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s)) {
      case (Empty, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), Empty)))
      case (Empty, Cons(h, t)) => Some(((None, Some(h())), (Empty, t())))
    }
  }


  def hasSubsequence2[A](s: Stream[A]): Boolean = {
    val bsubs: Stream[Boolean] = unfold(this) {
      case Empty => None
      case curr@Cons(_, t) => Some(curr.startsWith(s), t())
    }
    bsubs.exists(identity)
  }

  /*
  s1: AEE
  s2: AAA

  s1: AAA
  s2: AEE
   */
  def startsWith[B](s: Stream[B]): Boolean = {
    //this.zipAll(s).forAll(e => e._2.isEmpty || (!e._2.isEmpty && e._1 == e._2))
    this.zipAll(s).takeWhile(!_._2.isEmpty).forAll(e => e._1 == e._2)
  }

  /*
  An additional Empty is needed, e.g:
  Stream(1,2).tails => [Stream(1, 2), Stream(2), Stream()]
  The last element of `tails` is always the empty `Stream`
  */
  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Empty => None
      case s@Cons(_, t) => Some(s, t())
    }.append(Stream(empty)) // append(Stream()) is not enough
  }

  def hasSubsequence[A](s: Stream[A]): Boolean = {
    //this.tails.foldRight(false)((t, acc) => t.startsWith(s) || acc)
    this.tails.exists(_.startsWith(s))
  }

  /*
  Produces a collection containing cumulative results of applying the operator
  going right to left. The head of the collection is the last cumulative result.

  def foldRight[B](z: => B)(f: (A, => B) => B): B
   */
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, Stream(z))) { (a, _b) =>
      // `lazy val` to cache the by-name parameter
      lazy val b = _b
      val b2 = f(a, b._1)
      (b2, cons(b2, b._2))
    }._2
  }

  def scanRight2[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
    case Empty => Stream(z)
    case Cons(h, t) => {
      val bs = t().scanRight(z)(f)
      cons(f(h(), bs.headOption.get), bs)
    }
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  // smart constructors

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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))


  val fib: Stream[Int] = {
    def go(i: Int, j: Int): Stream[Int] = {
      cons(i, go(j, i + j))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some(v) => cons(v._1, unfold(v._2)(f))
  }

  //val onesViaUnfold: Stream[Int] = unfold(1)(i => Some((1, i)))
  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  //def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(s => Some(a, s))
  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def fibViaUnfold: Stream[Int] =
    unfold((0, 1)) {
      case (v1, v2) => Some((v1 + v2, (v2, v1 + v2)))
    }
}