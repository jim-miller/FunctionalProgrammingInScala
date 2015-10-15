package com.jimmiller.chapter03

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) {
      Nil
    } else {
      Cons(as.head, apply(as.tail: _*))
    }
  }

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

  /** Returns a List consisting of all but the last element of a List */
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) ⇒ Nil
    case Cons(h, t) ⇒ Cons(h, init(t))
    case Nil ⇒ sys.error("Error: empty list encountered")
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) ⇒ B): B = as match {
    case Nil ⇒ z
    case Cons(x, xs) ⇒ f(x, foldRight(xs,z)(f))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil ⇒ 0
    case Cons(x, xs) ⇒ x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil ⇒ 1.0
    case Cons(x, xs) ⇒ x * product(xs)
  }

  def sum2(ints: List[Int]): Int = foldRight(ints, 0)(_+_)

  def product2(ds: List[Double]): Double = foldRight(ds, 1.0)(_*_)

  def setHead[A](l: List[A], h: A) = Cons(h, l)

  def tail[A](l: List[A]): List[A] = drop(l, 1)

  def length[A](as: List[A]): Int = {
    foldRight(as,0)((_,t) ⇒ t + 1)
  }

}
