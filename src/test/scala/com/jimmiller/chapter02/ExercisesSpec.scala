package com.jimmiller.chapter02

import com.jimmiller.chapter02.Exercise2_1._
import org.scalatest.FlatSpecLike

class ExcercisesSpec extends FlatSpecLike {
  "Excercise 2.1" should "produce a fibonacci sum for a given integer" in {
    assert(Exercise2_1.fib(0) === 0)
    assert(Exercise2_1.fib(1) === 1)
    assert(Exercise2_1.fib(5) === 5)
    assert(Exercise2_1.fib(9) === 34)
  }
}
