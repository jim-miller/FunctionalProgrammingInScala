package com.jimmiller.chapter03

import com.jimmiller.chapter03.List._
import org.scalatest.{FlatSpecLike, Matchers}

class ExercisesSpec extends FlatSpecLike with Matchers {

  "Exercise 3.1" should "produce the predicted output" in {
    val matchedResult = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    matchedResult shouldBe 3
  }

  "Exercise 3.2" should "implement the function tail for removing the first element of a List" in {
    val origList = List("a", "b", "c")
    val expectedList = List("b", "c")

    tail(origList) shouldBe expectedList
  }

  "Exercise 3.3" should "implement the function setHead for replacing the first element of a List" in {
    val origList = List(1, 2, 3, 4)
    val expectedList = List(0, 1, 2, 3, 4)

    setHead(origList, 0) shouldEqual expectedList
  }

  "Exercise 3.4" should "generalize tail into a drop function which removes the first n elements from a list" in {
    val origList = List(1, 2, 3, 4)

    drop(origList, 2) shouldEqual List(3, 4)
  }

  "Exercise 3.5" should "implement dropWhile, which removes elements from the List prefix as long as they match a predicate" in {
    val origList = List(1.2, 1.3, 1.4, 1.5, 1.6)

    dropWhile(origList)(x ⇒ x < 1.45) shouldEqual List(1.5, 1.6)
  }

  "Exercise 3.6" should "implement init to return a List consisting of all but the last element of a List" in {
    val given = List(1,2,3,4)

    init(given) shouldEqual List(1,2,3)
  }

  "Exercise 3.7" should "discuss if product, implemented using foldRight, can be short-circuited if it encounters 0.0" in {
    foldRight(List(1,2,3,4), 1)(_*_) shouldBe 24
    productUsingFoldRight(List(1,2,3)) shouldBe 6
    sumUsingFoldRight(List(1,2,3,4,5,6)) shouldBe 21

    // Short circuiting a product operation encountering 0.0 can't be done in the current
    // implementation since f()'s arguments have to be evaluated beforehand
  }

  "Exercise 3.8" should "demonstrate constructing a List via foldRight" in {
    // See ExerciseWorksheet for demonstration
  }

  "Exercise 3.9" should "compute the length of a list using foldRight" in {
    List.lengthUsingFoldRight(List("a","b","c","d","e")) shouldBe 5
  }

  "Exercise 3.10" should "implement foldLeft in a tail-recursive fashion" in {
    foldLeft(List(1,2,3,4,5), 1.0)(_*_) shouldBe 120
  }

  "Exercise 3.11" should "implement sum, length, and product using foldLeft" in {
    sumUsingFoldLeft(List(1,2,3,4,5,6)) shouldBe 21
    productUsingFoldLeft(List(5,5,5)) shouldBe 125
    lengthUsingFoldLeft(List("Doc", "Grumpy", "Happy", "Sleepy", "Bashful", "Sneezy", "Dopey")) shouldBe 7
  }

  "Exercise 3.12" should "implement a reverse function that relies on folding" in {
    reverseUsingFoldLeft(List(1,2,3)) shouldBe List(3,2,1)
  }

  "Exercise 3.13" should "expresses foldLeft as an operation of foldRight and vice versa" in {
    foldLeftUsingFoldRight(List(6,6), 0)(_+_) shouldBe 12
    foldRightUsingFoldLeft(List(6,6), 0)(_+_) shouldBe 12
  }

  "Exercise 3.14" should "implement addend using foldRight" in {
    appendUsingFoldRight(List(1,2,3), List(4)) shouldBe List(1,2,3,4)
  }

  "Exercise 3.15" should "implement concat in a linear way" in {
    concat(List(List(1,2,3), List(4,5,6))) shouldBe List(1,2,3,4,5,6)
  }

  "Exercise 3.16" should "transform a list of integers by adding 1 to each element" in {
    addOneToEachInteger(List(1,2,3)) shouldBe List(2,3,4)
  }
}
