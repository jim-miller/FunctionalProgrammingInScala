package com.jimmiller.chapter03

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def addOneToEachInteger(ints: List[Int]): List[Int] = {
    foldRight(ints, Nil: List[Int])((x, xs) ⇒ Cons(x + 1, xs))
  }

  def appendUsingFoldRight[A](as: List[A], a: List[A]): List[A] =
    foldRight(as, a)(Cons(_, _))

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) {
      Nil
    } else {
      Cons(as.head, apply(as.tail: _*))
    }
  }

  def concat[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(appendUsingFoldRight)

  def drop[A](l: List[A], i: Int): List[A] = l match {
    case Cons(_, t) ⇒ {
      if (i > 0) {
        drop(t, i - 1)
      } else {
        l
      }

    }
    case Nil ⇒ sys.error("Error: empty list encountered")
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, _) ⇒ {
      if (f(h)) {
        dropWhile(tail(l))(f)
      } else {
        l
      }
    }
    case Nil ⇒ sys.error("Cannot drop from empty List")
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((x, xs) ⇒ if (f(x)) {
      Cons(x, xs)
    } else {
      filter(xs)(f)
    })
  }

  /**
   * flatMap works like map except that the function given will return a list instead of a single
   * result, and that list should be inserted into the final resulting list.
   */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil ⇒ z
    case Cons(x, xs) ⇒ foldLeft(xs, f(z, x))(f)
  }

  def foldLeftUsingFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B = as match {
    case Nil ⇒ z
    case Cons(x, xs) ⇒ f(x, foldRight(xs, z)(f))
  }

  def foldRightUsingFoldLeft[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B = {
    foldLeft(reverseUsingFoldLeft(as), z)((b, a) => f(a, b))
  }

  /** Returns a List consisting of all but the last element of a List */
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) ⇒ Nil
    case Cons(h, t) ⇒ Cons(h, init(t))
    case Nil ⇒ sys.error("Error: empty list encountered")
  }

  def lengthUsingFoldLeft[A](as: List[A]): Int = {
    foldLeft(as, 0)((h, _) ⇒ h + 1)
  }

  def lengthUsingFoldRight[A](as: List[A]): Int = {
    foldRight(as, 0)((_, t) ⇒ t + 1)
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((x, xs) ⇒ Cons(f(x), xs))
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil ⇒ 1.0
    case Cons(x, xs) ⇒ x * product(xs)
  }

  def productUsingFoldLeft(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def productUsingFoldRight(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

  def reverseUsingFoldLeft[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((h, t) ⇒ Cons(t, h))
  }

  def setHead[A](l: List[A], h: A) = Cons(h, l)

  def sum(ints: List[Int]): Int = ints match {
    case Nil ⇒ 0
    case Cons(x, xs) ⇒ x + sum(xs)
  }

  def sumUsingFoldLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def sumUsingFoldRight(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

  def tail[A](l: List[A]): List[A] = drop(l, 1)

  def turnListDoubleToListString(ds: List[Double]): List[String] = {
    foldRight(ds, Nil: List[String])((x, xs) ⇒ Cons(x.toString, xs))
  }
}
