package com.jimmiller.chapter02

object Exercise2_1 {

  def fib(n: Int): Int = {

    @annotation.tailrec
    def go(n: Int, prev: Int, next: Int): Int = n match {
      case 0 ⇒ prev
      case 1 ⇒ next
      case _ ⇒ go(n - 1, next, (next + prev))
    }

    go(n, 0, 1)
  }
}

object Exercise2_2 {
  def isSorted[A](as: Array[A], ordered: (A,A) ⇒ Boolean): Boolean = {
    !as.sliding(2).map(a ⇒ ordered(a(0), a(1))).contains(false)
  }
}

object Exercise2_3 {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a ⇒ (b ⇒ f(a, b))
  }
}

object Exercise2_4 {
  def uncurry[A,B,C](f: A ⇒ B ⇒ C): (A, B) ⇒ C = {
    (a, b) ⇒ f(a)(b)
  }
}

object Exercise2_5 {
  def compose[A,B,C](f: B ⇒ C, g: A ⇒ B): A ⇒ C = {
    a ⇒ f(g(a))
  }
}

