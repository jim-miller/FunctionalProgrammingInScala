package com.jimmiller.chapter05

import org.scalatest.{FreeSpec, Matchers}

class Chapter05ExercisesSpec extends FreeSpec with Matchers {

  "Exercise 5.1" - {
    "should convert a Stream to a List" in {
      val s = Stream((1 + 1), (2 + 2), (3 + 3))
      s.toList shouldBe List(2, 4, 6)
    }
  }

  "Exercise 5.2" - {
    "should implement take(n) for returning the first n elements of a Stream" in {
      Stream("a", "b", "c", "d").take(3).toList shouldBe List("a", "b", "c") // We haven't implemented Stream equality
    }

    "should implement drop(n) for skipping the first n elements of a Stream" in {
      Stream(1, 2, 3, 4, 5).drop(4).toList shouldBe List(5)
      Stream("a", "b", "c", "d").drop(2).toList shouldBe List("c", "d")
    }

  }

  "Exercise 5.3" - {
    "should implement takeWhile for returning all starting elements of a Stream that match a given predicate" in {
      Stream(10, 20, 30, 40).takeWhile(_ < 25).toList shouldBe List(10, 20)
    }
  }

  "Exercise 5.4" - {
    "should implement forAll which checks that all elements in the Stream match a given predicate" in {
      Stream("abc", "123", "yes").forAll(_.length == 3) shouldBe true
      Stream("Scala", "Ruby", "Java").forAll(_.length == 4) shouldBe false
    }
  }

  "Exercise 5.5" - {
    "should use foldRight to implement takeWhile" in {
      Stream(10, 20, 30, 40).takeWhileFR(_ < 25).toList shouldBe List(10, 20)
    }
  }

  "Exercise 5.6" - {
    "should implement headOption using foldRight" in {
      Stream("a", "b", "c").headOption shouldBe Option("a")
    }
  }

  "Exercise 5.7" - {
    "should implement map using foldRight" in {
      Stream(1, 2, 3).map(_ * 2).toList shouldBe List(2, 4, 6)
    }
  }

  "Exercise 5.8" - {
    "should implement constant which returns an infinite Stream of a given value" in {
      Stream.constant(1).take(19).toList shouldBe List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    }
  }

  "Exercise 5.9" - {
    "should implement a function that generates an infinite stream of integers, starting from n" in {
      Stream.from(8).take(5).toList shouldBe List(8, 9, 10, 11, 12)
    }
  }

  "Exercise 5.10" - {
    "should implement a function fibs that generates the infinite stream of Fibonacci numbers" in {
      Stream.fibs.take(8).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13)
    }
  }

  "Exercise 5.11" - {
    "should implement a more general stream-building function called unfold" in {
      Stream.unfold()
    }
  }

  "Exercise 5.12" - {
    "should implement fibs in terms of unfold" in pending
    "should implement from in terms of unfold" in pending
    "should implement constant in terms of unfold" in pending
    "should implement ones in terms of unfold" in pending
  }

  "Exercise 5.13" - {
    "should implement map using unfold" in pending
    "should implement take using unfold" in pending
    "should implement takeWhile using unfold" in pending
    "should implement zipWith using unfold" in pending
    "should implement zipAll using unfold" in pending
  }

  "Exercise 5.14" - {
    "should implement startsWith using functions from chapter 5" in pending
  }

  "Exercise 5.15" - {
    "should implement tails using unfold" in pending
  }

  "Exercise 5.16" - {
    "should implement scanRight which acts like foldRight but returns a stream of the intermediate results" in pending
  }
}
