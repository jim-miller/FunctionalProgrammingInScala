package com.jimmiller.chapter03

import org.scalatest.{FlatSpecLike, Matchers}

class ExercisesSpec extends FlatSpecLike with Matchers {

  "Exercise 3.1" should "produce the predicted output" in {
    val matchedResult = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    matchedResult shouldBe 3
  }

  "Exercise 3.2" should "implement the function tail for removing the first element of a List" in {
    val origList = List("a", "b", "c")
    val expectedList = List("b", "c")

    List.tail(origList) shouldBe expectedList
  }

  "Exercise 3.3" should "implement the function setHead for replacing the first element of a List" in {
    val origList = List(1, 2, 3, 4)
    val expectedList = List(0, 1, 2, 3, 4)

    List.setHead(origList, 0) shouldEqual expectedList
  }
}
