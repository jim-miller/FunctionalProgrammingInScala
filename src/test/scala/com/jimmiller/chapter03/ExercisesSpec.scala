package com.jimmiller.chapter03

import com.jimmiller.chapter03.List._
import com.jimmiller.chapter03.Tree._
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

  "Exercise 3.5" should
  "implement dropWhile, which removes elements from the List prefix as long as they match a predicate" in {
    val origList = List(1.2, 1.3, 1.4, 1.5, 1.6)

    dropWhile(origList)(x ⇒ x < 1.45) shouldEqual List(1.5, 1.6)
  }

  "Exercise 3.6" should "implement init to return a List consisting of all but the last element of a List" in {
    val given = List(1, 2, 3, 4)

    init(given) shouldEqual List(1, 2, 3)
  }

  "Exercise 3.7" should
  "discuss if product, implemented using foldRight, can be short-circuited if it encounters 0.0" in {
    foldRight(List(1, 2, 3, 4), 1)(_ * _) shouldBe 24
    productUsingFoldRight(List(1, 2, 3)) shouldBe 6
    sumUsingFoldRight(List(1, 2, 3, 4, 5, 6)) shouldBe 21

    // Short circuiting a product operation encountering 0.0 can't be done in the current
    // implementation since f()'s arguments have to be evaluated beforehand
  }

  "Exercise 3.8" should "demonstrate constructing a List via foldRight" in {
    // See ExerciseWorksheet for demonstration
  }

  "Exercise 3.9" should "compute the length of a list using foldRight" in {
    List.lengthUsingFoldRight(List("a", "b", "c", "d", "e")) shouldBe 5
  }

  "Exercise 3.10" should "implement foldLeft in a tail-recursive fashion" in {
    foldLeft(List(1, 2, 3, 4, 5), 1.0)(_ * _) shouldBe 120
  }

  "Exercise 3.11" should "implement sum, length, and product using foldLeft" in {
    sumUsingFoldLeft(List(1, 2, 3, 4, 5, 6)) shouldBe 21
    productUsingFoldLeft(List(5, 5, 5)) shouldBe 125
    lengthUsingFoldLeft(List("Doc", "Grumpy", "Happy", "Sleepy", "Bashful", "Sneezy", "Dopey")) shouldBe 7
  }

  "Exercise 3.12" should "implement a reverse function that relies on folding" in {
    reverseUsingFoldLeft(List(1, 2, 3)) shouldBe List(3, 2, 1)
  }

  "Exercise 3.13" should "expresses foldLeft as an operation of foldRight and vice versa" in {
    foldLeftUsingFoldRight(List(6, 6), 0)(_ + _) shouldBe 12
    foldRightUsingFoldLeft(List(6, 6), 0)(_ + _) shouldBe 12
  }

  "Exercise 3.14" should "implement addend using foldRight" in {
    appendUsingFoldRight(List(1, 2, 3), List(4)) shouldBe List(1, 2, 3, 4)
  }

  "Exercise 3.15" should "implement concat in a linear way" in {
    concat(List(List(1, 2, 3), List(4, 5, 6))) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  "Exercise 3.16" should "transform a list of integers by adding 1 to each element" in {
    addOneToEachInteger(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }

  "Exercise 3.17" should "turn each value in a List[Double] into a String" in {
    turnListDoubleToListString(List(2.1, 4.2, 5.3)) shouldBe List("2.1", "4.2", "5.3")
  }

  "Exercise 3.18" should "implement map that modifies each element in a list while maintaining the list structure" in {
    map(List(1, 2, 3))(_ * 10) shouldBe List(10, 20, 30)
  }

  "Exercise 3.19" should "implement filter that removes elements from a list unless they satisfy a given predicate" in {
    filter(List(1, 2, 3, 4))(_ % 2 == 0) shouldBe List(2, 4)
  }

  "Exercise 3.20" should "implement flatMap" in {
    flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  "Exercise 3.21" should "implement filter using flatMap" in {
    filterUsingFlatMap(List("a", "ab", "abc"))(_.contains("b")) shouldBe List("ab", "abc")
  }

  "Exercise 3.22" should "accept two lists and construct a new list by adding corresponding elements" in {
    zipIntLists(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
    zipIntLists(List(1, 3, 5), List(2, 4)) shouldBe List(3, 7)
  }

  "Exercise 3.23" should "generalize a zip function" in {
    val alphList = List("a", "b", "c")
    val numList = List(1, 2, 3)
    val expected = List(List("a", 1), List("b", 2), List("c", 3))

    zipWith(alphList, numList)((a, b) ⇒ Cons(a, Cons(b, Nil))) shouldBe expected
  }

  "Exercise 3.24" should
  "implement hasSubsequence for checking whether a List contains another List as a subsequence" in {
    hasSubsequence(List(1, 2, 3, 4), List(1)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4), List(3)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4), List(1, 2)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4), List(2, 3)) shouldBe true
    hasSubsequence(List(1, 2, 3, 4), List(2, 3, 4)) shouldBe true

    hasSubsequence(List(1, 2, 3, 4), List(5)) shouldBe false
    hasSubsequence(List(1, 2, 3, 4), List(1, 3, 4)) shouldBe false
  }

  "Exercise 3.25" should "implement a size function that counts the number of nodes (leaves and branches) in a tree" in
  {
    val singleLeaf = Leaf(1)
    val twoLeafTree = Branch(singleLeaf, singleLeaf)
    val imbalancedTree = Branch(twoLeafTree, singleLeaf)

    Tree.size(singleLeaf) shouldBe 1
    Tree.size(twoLeafTree) shouldBe 3
    Tree.size(imbalancedTree) shouldBe 5
  }

  "Exercise 3.26" should "implement a function maximum that returns the maximum element in a Tree[Int]" in {
    Tree.maximum(Branch(Leaf(5),Leaf(8))) shouldBe 8
  }

  "Exercise 3.27" should "implement depth to return the maximum path length from the root of a tree to any leaf" in {
    Tree.depth(Branch(Leaf(1),Leaf(2))) shouldBe 1
    Tree.depth(Branch(Leaf(1),Branch(Leaf(2), Branch(Leaf(3),Leaf(4))))) shouldBe 3
  }
}
