package fpinscala.datastructures

import scala.annotation.tailrec

object ListApp extends App {
  println(List.x)

}

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  //def tail[A](l: List[A]): List[A] = sys.error("todo")
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, rest) => rest
      case _ => throw new IllegalArgumentException
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Cons(_, rest) => Cons(h, rest)
      case _ => throw new IllegalArgumentException
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) {
      l
    } else {
      l match {
        case Cons(_, rest) => drop(rest, n - 1)
        case _ => throw new IllegalArgumentException
      }
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, rest) =>
        if (f(h)) {
          dropWhile(rest, f)
        } else {
          l
        }
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(h, Nil) => Nil
      case Cons(h, rest) => Cons(h, init(rest))
      case _ => throw new IllegalArgumentException
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => acc + 1)
  }

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Cons(h, rest) => foldLeft(rest, f(z, h))(f)
      case Nil => z
    }
  }

  def reverse2[A](l: List[A]): List[A] = {
    @tailrec
    def helper(lst: List[A], acc:List[A]): List[A] = {
      lst match {
        case Cons(h, rest) => helper(rest, Cons(h, acc))
        case Nil => Nil
      }
    }
    helper(l, Nil)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))
  }

  // have the drawback of iterating twice
  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(l), z)((a, b) => f(b, a))
  }

  // have the drawback of iterating twice
  // but having the advantage of @tailrec brought by foldLeft
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((b, a) => f(a, b))
  }


  // in essence the same `append`
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
    reverse(foldLeft(reverse(a2), reverse(a1))((b, a) => Cons(a, b)))
  }


  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil:List[A])(append)
  }

  def map2[A,B](l: List[A])(f: A => B): List[B] = {
    @tailrec
    def map_helper(lst: List[A], acc: List[B]): List[B] = {
      lst match {
        case Cons(h, rest) => map_helper(rest, Cons(f(h), acc))
        case Nil => acc
      }
    }
    map_helper(l, Nil)
  }

  // instead of vanilla foldRight, using foldRightViaFoldLeft to avoid a
  // potential stack overflow.
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRightViaFoldLeft(l, Nil: List[B])((a, b) => Cons(f(a), b))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRightViaFoldLeft(l, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    concat(map(l)(f))
  }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  def addPairwise(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (_, Nil) => l1
      case (Nil, _) => l2
      case (Cons(a, ra), Cons(b, rb)) => Cons(a + b, addPairwise(ra, rb))
    }
  }

  // unlike addPairwise, it stops once seeing Nil in either list
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(a, ra), Cons(b, rb)) => Cons(f(a, b), zipWith(ra, rb)(f))
    }
  }

  def hasSubsequence[A](l1: List[A], l2: List[A]): Boolean = {
    def cmp(l1: List[A], l2: List[A]): Boolean = {
      (l1, l2) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(a, ra), Cons(b, rb)) => a == b && cmp(ra, rb)
      }
    }

    if (cmp(l1, l2)) true
    else hasSubsequence(tail(l1), l2)
  }
}
