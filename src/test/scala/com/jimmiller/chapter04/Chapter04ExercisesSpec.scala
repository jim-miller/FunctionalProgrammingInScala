package com.jimmiller.chapter04

import org.scalatest.{BeforeAndAfter, Matchers, FreeSpec}

class Chapter04ExercisesSpec extends FreeSpec with Matchers {

  "Exercise 4.1" - {
    "should implement map for Option" in {
      Some(1).map(_ + 1) shouldBe Some(2)
    }
    "should implement flatMap for Option" in {
      Some(1).flatMap(Some(_)) shouldBe Some(1)
    }
    "should implement getOrElse for Option" in {
      None.getOrElse(1) shouldBe 1
      Some(2).getOrElse(1) shouldBe 2
    }
    "should implement orElse for Option" in {
      None.orElse(Some(1)) shouldBe Some(1)
      Some(2).orElse(Some(3)) shouldBe Some(2)
    }
    "should implement filter for Option" in {
      Some(3).filter(_ % 2 == 0) shouldBe None
      Some(4).filter(_ % 2 == 0) shouldBe Some(4)
    }
  }

  "Exercise 4.2" - {
    "should implement a variance function in terms of flatMap" in {
      // Variance is the average of the squared differences from the Mean.
      // I.e. we compute the mean of the set then for each number x, subtract the mean and
      // square the result then average the squared differences we just rolled up.

      Option.variance(List(2.0, 2.0)) shouldBe Some(0.0)
      Option.variance(List(1.0, 7.0)) shouldBe Some(9.0)

      Option.variance(List()) shouldBe None
    }
  }

  "Exercise 4.3" - {
    "should implement a map2 function that combines two Option values using a binary function" in {
      Option.map2(Some(1), Some(2))(_ + _) shouldBe Some(3)
      Option.map2(None, Some(2))((a, b) ⇒ (b, a)) shouldBe None
    }
  }

  "Exercise 4.4" - {
    "should implement a sequence function that combines a list of Options into one Option containing a " +
    "list of all the Some values in the original list" in {
      Option.sequence(List(Some("a"), Some("b"), Some("c"))) shouldBe Some(List("a", "b", "c"))
      Option.sequence(List(Some("a"), None, Some("c"))) shouldBe None
    }
  }

  "Exercise 4.5" - {
    "should implement a traverse function that sequences the results of a map" in {
      Option.traverse(List(1, 2))(Some(_)) shouldBe Some(List(1, 2))
    }
  }

  "Exercise 4.6" - {
    "should implement a function map in Either that operates on the Right value." in {
      val e: Either[String,Int] = Right(1)

      e.map(_ + 1) shouldBe Right(2)
    }
    "should implement flatMap in Either that operates on the Right value." in {
      Right(1).flatMap(Right(_)) shouldBe Right(1)
    }
    "should implement orElse in Either, that operates on the Right value." in {
      Left("You shall not pass!").orElse(Left("Failure")) shouldBe Left("Failure")
      Right("Success").orElse(Left("Failure")) shouldBe Right("Success")
    }
    "should implement  map2 in Either that operates on the Right value." in {
      Right("Success").map2(Right(" is awesome"))(_ + _) shouldBe Right("Success is awesome")
      Right("Success").map2(Left("Error"))(_ + _) shouldBe Left("Error")
    }
  }

  "Exercise 4.7" - {
    "should implement sequence for Either" in {
      Either.sequence(List(Right("a"), Right("b"), Right("c"))) shouldBe Right(List("a", "b", "c"))
      Either.sequence(List(Right("a"), Left("ERROR"), Right("c"))) shouldBe Left("ERROR")
    }
    "should implement traverse for Either" in {
      Either.traverse(List(1, 2))(Right(_)) shouldBe Right(List(1, 2))
      Either.traverse(List(1, 2))(Left(_)) shouldBe Left(1) // Stops at the first error
    }
  }

  "Exercise 4.8" - {
    "should explain how you would report both errors from map2" in {
      /*
      You would need another object to store and report back on the errors
       */
    }
  }
}
