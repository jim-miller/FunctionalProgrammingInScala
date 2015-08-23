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

    // Ints
    val ascSortedIntArray = Array(1,2,3)
    val decSortedIntArray = Array(9,6,3)
    val unSortedIntArray = Array(5,2,7)

    def ascSortIntF = (x: Int, y: Int) ⇒ x < y
    def decSortIntF = (x: Int, y: Int) ⇒ x > y

    assert(isSorted(ascSortedIntArray, ascSortIntF) === true)
    assert(isSorted(decSortedIntArray, decSortIntF) === true)

    assert(isSorted(ascSortedIntArray, decSortIntF) === false)
    assert(isSorted(decSortedIntArray, ascSortIntF) === false)

    assert(isSorted(unSortedIntArray, ascSortIntF) === false)
    assert(isSorted(unSortedIntArray, decSortIntF) === false)

    // Strings
    val ascSortedStrings = Array("Apple", "Banana", "Cherry", "Date", "Elderberry")
    val decSortedStrings = Array("Zebra", "Yak", "Horse", "Antelope")
    val unSortedStrings = Array("Ruby", "Haskell", "Scala", "Java")

    def ascSortStringF = (x: String, y: String) ⇒ x < y
    def decSortStringF = (x: String, y: String) ⇒ x > y

    assert(isSorted(ascSortedStrings, ascSortStringF) === true)
    assert(isSorted(decSortedStrings, decSortStringF) === true)

    assert(isSorted(ascSortedStrings, decSortStringF) === false)
    assert(isSorted(decSortedStrings, ascSortStringF) === false)

    assert(isSorted(unSortedStrings, ascSortStringF) === false)
    assert(isSorted(unSortedStrings, decSortStringF) === false)

  }
}
