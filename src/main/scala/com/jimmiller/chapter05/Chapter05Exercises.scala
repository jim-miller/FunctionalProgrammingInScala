package com.jimmiller.chapter05

sealed trait Stream[+A] {

  def toListNotStackSafe: List[A] = this match {
    case Cons(h, t) ⇒ h() :: t().toListNotStackSafe
    case _ ⇒ List()
  }

  def toList: List[A] = {

    @annotation.tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => loop(t(), h() :: acc)
      case _ => acc
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

  def empty[A]: Stream[A] = Empty

}