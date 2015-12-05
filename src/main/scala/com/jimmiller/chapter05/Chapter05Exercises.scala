package com.jimmiller.chapter05

import Stream._

sealed trait Stream[+A] {

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 ⇒ t().drop(n - 1)
    case _ ⇒ this
  }

  def foldRight[B](z: ⇒ B)(f: (A, ⇒ B) ⇒ B): B = this match {
    case Cons(h, t) ⇒ f(h(), t().foldRight(z)(f))
    case _ ⇒ z
  }

  def forAll(f: (A) ⇒ Boolean): Boolean = {
    foldRight(true)((h, t) ⇒ f(h) && t)
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((h, _) ⇒ Some(h))
  }

  def map[B](f: A => B) = {
    foldRight(empty[B])((h, t) => cons(f(h), t))
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 ⇒ cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 ⇒ cons(h(), empty)
    case _ ⇒ empty
  }

  def takeWhile(f: (A) ⇒ Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) ⇒ cons(h(), t() takeWhile f)
    case _ ⇒ empty
  }

  def takeWhileFR(f: (A) ⇒ Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) ⇒ if (f(h)) cons(h, t) else empty)
  }

  def toListNotStackSafe: List[A] = this match {
    case Cons(h, t) ⇒ h() :: t().toListNotStackSafe
    case _ ⇒ List()
  }

  def toList: List[A] = {

    @annotation.tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) ⇒ loop(t(), h() :: acc)
      case _ ⇒ acc
    }

    loop(this, List()).reverse
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () ⇒ A, t: () ⇒ Stream[A]) extends Stream[A]

object Stream {

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) {
      Empty
    } else {
      cons(as.head, apply(as.tail: _*))
    }
  }

  def cons[A](hd: ⇒ A, tl: ⇒ Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() ⇒ head, () ⇒ tail)
  }

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def empty[A]: Stream[A] = Empty

  def from(i: Int): Stream[Int] = {
    cons(i, from(i + 1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  val fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

}