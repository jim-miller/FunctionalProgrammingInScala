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
    "should implement take(n) for returning the first n elements of a Stream" in pending
    "should implement drop(n) for skipping the first n elements of a Stream" in pending
  }

  "Exercise 5.3" - {
    "should implement takeWhile for returning all starting elements of a Stream that match a given predicate" in pending
  }

  "Exercise 5.4" - {
    "should implement forAll which checks that all elements in the Stream match a given predicate" in pending
  }

  "Exercise 5.5" - {
    "should use foldRight to implement takeWhile" in pending
  }

  "Exercise 5.6" - {
    "should implement headOption using foldRight" in pending
  }

  "Exercise 5.7" - {
    "should implement map using foldRight" in pending
  }

  "Exercise 5.8" - {
    "should implement constant which returns an infinite Stream of a given value" in pending
  }

  "Exercise 5.9" - {
    "should implement a function that generates an infinite stream of integers, starting from n" in pending
  }

  "Exercise 5.10" - {
    "should implement a function fibs that generates the infinite stream of Fibonacci numbers" in pending
  }

  "Exercise 5.11" - {
    "should implement a more general stream-building function called unfold" in pending
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
