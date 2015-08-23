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
    true
  }
}
