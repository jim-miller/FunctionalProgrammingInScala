package com.jimmiller.chapter03

import fpinscala.datastructures.List
import org.scalatest.FlatSpecLike

class ExercisesSpec extends FlatSpecLike {
  "Exercise 3.1" should "produce the predicted output" in {
        //  val x = List(1, 2, 3, 4, 5) match {
        //    case Cons(x, Cons(2, Cons(4, _))) => x
        //    case Nil => 42
        //    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        //    case Cons(h, t) => h + sum(t)
        //    case _ => 101
        //  }
    assert(List.x == 1 + 2)
  }
}
