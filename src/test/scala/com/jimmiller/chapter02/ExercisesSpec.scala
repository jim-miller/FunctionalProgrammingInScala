package com.jimmiller.chapter02

import com.jimmiller.chapter02.Exercise2_2._
import org.scalatest.FlatSpecLike

class ExercisesSpec extends FlatSpecLike {
  "Exercise 2.1" should "produce a fibonacci sum for a given integer" in {
    assert(Exercise2_1.fib(0) === 0)
    assert(Exercise2_1.fib(1) === 1)
    assert(Exercise2_1.fib(5) === 5)
    assert(Exercise2_1.fib(9) === 34)
  }

  "Exercise 2.2" should "checks whether an Array[A] is sorted according to a given comparison function" in {

    val ascSortedIntArray = Array(1,2,3)
    val decSortedIntArray = Array(9,6,3)
    val unSortedIntArray = Array(5,2,7)

    def ascSortF = (x: Int, y: Int) ⇒ x < y
    def decSortF = (x: Int, y: Int) ⇒ x > y

    assert(isSorted(ascSortedIntArray, ascSortF) === true)
    assert(isSorted(decSortedIntArray, decSortF) === true)

    assert(isSorted(ascSortedIntArray, decSortF) === false)
    assert(isSorted(decSortedIntArray, ascSortF) === false)

    assert(isSorted(unSortedIntArray, ascSortF) === false)
    assert(isSorted(unSortedIntArray, decSortF) === false)
  }
}
