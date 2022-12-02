package com.rockthejvm.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  /**
   * Easy problems
   */
  // get element at a given index
  def apply(index: Int): T

  // the size of the list
  def length: Int

  // reverse the list
  def reverse: RList[T]

  // concatenate another list to this one
  def ++[S >: T](anotherList: RList[S]): RList[S]

  // remove an element at a given index, return a NEW list
  def removeAt(index: Int): RList[T]

  // duplicate each element a number of times in a row
  def duplicateEach(k: Int): RList[T]

  // rotation by a numebr of positions to the left
  def rotate(k: Int): RList[T]

  
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  /**
   * Easy problems
   */
  // get element at a given index
  override def apply(index: Int): Nothing = throw new NoSuchElementException

  // the size of the list
  override def length: Int = 0

  // reverse the list
  override def reverse: RList[Nothing] = RNil

  // append another list
  def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  // remove an element at a given index, return a NEW list
  override def removeAt(index: Int): RList[Nothing] = RNil

  // duplicate each element a number of times in a row
  override def duplicateEach(k: Int): RList[Nothing] = RNil

  // rotation by a numebr of positions to the left
  override def rotate(k: Int): RList[Nothing] = RNil
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false
  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailrec(this, "") + "]"
  }

  /**
   * Easy problems
   */
  // get element at a given index

  // Initial time to solve: 9:11
//  override def apply(index: Int): T = {
//    if (index == 0) {
//      if (this.isEmpty) throw new NoSuchElementException
//      else head
//    } else {
//      if (tail.isEmpty) throw new NoSuchElementException
//      else tail.apply(index - 1) // can stack overflow for very long lists... needs tailrec (inner) function
//    }
//  }

  // Retry with tailrec: 20:01 after Daniel's indications
  // Works, but:
  // 1. for negative indices it iterates uselessly through the entire list
  // 2. the logic is overly complicated - 3 ifs instead of 1 - I should not add more ifs just to be overcautious
//  override def apply(index: Int): T = {
//    @tailrec
//    def applyTailrec(index: Int, list: RList[T]): T = {
//      if (index == 0) {
//        if (this.isEmpty) throw new NoSuchElementException
//        else list.head
//      } else {
//        if (tail.isEmpty) throw new NoSuchElementException
//        else applyTailrec(index - 1, list.tail)
//      }
//    }
//
//    applyTailrec(index, this)
//  }

  // Writing Daniel's solution, after seeing it - just checking my meemory :)
  // Final implementation time: 30:32
  // Complexity: O(min(n, index))
  // Lessons:
  // - only functions can be stack recursive or tail recursive, not methods!
  // - need to strive for the simplest thing that works:
  //    - do not overcomplicate
  //    - need to trust myself more
  // - careful at invariants
  // Final time: 37:27
  override def apply(index: Int): T = {
    @tailrec
    def applyTailrec(remaining: RList[T], currentIndex: Int): T = {
      if (currentIndex == index) remaining.head
      else applyTailrec(remaining.tail, currentIndex + 1)
    }

    if(index < 0) throw new NoSuchElementException
    applyTailrec(this, 0)
  }

  // TODO: the size of the list
  // Initial time to solve:: 6:46
  // Lessons:
  // - no need to use return :)
  // Complexity: O(n)
  // Final time: 14:27
  override def length: Int = {
    @tailrec
    def lengthTailrec(remaining: RList[T], result: Int): Int = {
      if (remaining.isEmpty)  result
      else lengthTailrec(remaining.tail, result + 1)
    }

    lengthTailrec(this, 0)
  }

  // TODO: reverse the list
  // Initial time to solve:: 5:47
  // Complexity: O(n)
  // Final time: 24:45 (one extra problem :D)
  override def reverse: RList[T] = {
    @tailrec
    def reverseTailrec(remaining: RList[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else reverseTailrec(remaining.tail, remaining.head :: acc)
    }

    reverseTailrec(this, RNil)
  }

  // TODO: append another list
  // Initial time to solve:: 7:56
  // O(n) -> Wrong
  // O(N + M)
  // length of this list: N
  // length of the other list: M
  // Final time: 19:23
  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def concatTailrec(remainingList: RList[S], acc: RList[S]): RList[S] = {
      if (remainingList.isEmpty) acc
      else concatTailrec(remainingList.tail, remainingList.head :: acc)
    }

//    concatTailrec(anotherList, this.reverse).reverse
    concatTailrec(this.reverse, anotherList) // Daniel's optimisation - faster - only 1 reverse
  }

  // TODO: remove an element at a given index, return a NEW list
  // Initial time to solve:: 23:33
  // Complexity: O(n)
  // Lessons:
  // - don't forget override - just for clarity - it is implicitly deduced where there is signature matching
  // - once we have a generic type T we do not add it again as param in inner methods - causes confusion -> we then have T and T^2 etc...
  // - careful at efficiencies - no of reverse operations - does not impact complexity, but maybe the (dominating) constant
  // - keep things together - a change in algorithm logic needs to be accounted on all other level of the existing algorithm - otherwise bugs appear ^^
  // - forgot an edge case: currentIndex != index && remaining.isEmpty == true
  // - possible simplification of existing logic after any change - e.g. above edge-case bugfix
  // - test on largeLists also
  // Final time: 58:26
   def removeAt(index: Int): RList[T] = {
    @tailrec
    def removeAtTailrec(remaining: RList[T], reversedPredecessors: RList[T], currentIndex: Int): RList[T] = {
      if (currentIndex == index) reversedPredecessors.reverse ++ remaining.tail
      else if (remaining.isEmpty) reversedPredecessors.reverse
      else removeAtTailrec(remaining.tail, remaining.head :: reversedPredecessors, currentIndex + 1)
    }

    if (index < 0) this
    else removeAtTailrec(this, RNil, 0)
  }

  // Initial time to solve: 10:20
  // O(N * K)
  // Final time: 30:23
  override def duplicateEach(k: Int): RList[T] = {
    @tailrec
    def duplicateElementTailrec(elem: T, acc: RList[T], n: Int, current: Int): RList[T] = {
      if (current == n) acc
      else duplicateElementTailrec(elem, elem :: acc, n, current + 1)
    }

    @tailrec
    def duplicateTailRec(remainingList: RList[T], acc: RList[T]): RList[T] = {
      if (remainingList.isEmpty) acc
      else duplicateTailRec(remainingList.tail, acc ++ duplicateElementTailrec(remainingList.head, RNil, k, 0))
    }

    if (k < 0) this
    else duplicateTailRec(this, RNil)
  }

  // rotation by a numebr of positions to the left
  // Initial time to solve: 15:20
  // O(n)
  // Final time: ~35:00
  override def rotate(k: Int): RList[T] = {
    val len = this.length
    val kReduced = k % len

    @tailrec
    def rotateTailrec(remaining: RList[T], reversedSelected: RList[T], index: Int): RList[T] = {
      if (index == kReduced) remaining ++ reversedSelected.reverse
      else rotateTailrec(remaining.tail, remaining.head :: reversedSelected, index + 1)
    }

    if (k < 0) this
    else rotateTailrec(this, RNil, 0)
  }
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    def convertToRListTailrec(remaining: Iterable[T], acc: RList[T]): RList[T] = {
      if (remaining.isEmpty) acc
      else convertToRListTailrec(remaining.tail, remaining.head :: acc)
    }

    convertToRListTailrec(iterable, RNil).reverse
  }
}

object ListProblems extends App {
  RNil.::(2) == 2 :: RNil
  val aSmallList = 1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)
//  val aSmallList_2 = 4 :: 5 :: RNil // RNil.::(3).::(2).::(1)
//  println(aSmallList)
//  println(aSmallList ++ aSmallList_2)
//  println(aSmallList ++ RNil)
//  println(RNil ++ aSmallList)
//  println(RNil ++ RNil)
//  println(aSmallList.removeAt(-1))
//  println(aSmallList.removeAt(0))
//  println(aSmallList.removeAt(1))
//  println(aSmallList.removeAt(2))
//  println(aSmallList.removeAt(4))
//  val largeList = RList.from(1 to 10000)
//  println(largeList.removeAt(4))
//
//  println(RNil.removeAt(3))

//  println((2 :: RNil).length)
//  println((2 :: RNil).reverse)
//  println(aSmallList.reverse)
//  println(aSmallList.reverse.reverse)
//  println(RNil.reverse)

//  val largeList = RList.from(1 to 10000)
//  println(largeList)
//
////  println(aSmallList.apply(0))
////  println(aSmallList.apply(1))
////  println(aSmallList.apply(2))
////  println(aSmallList.apply(-1))
//    println(largeList.apply(9876))
//    println(largeList.length)
//    println(largeList.reverse)
//  val emptyList = RNil
//  println(emptyList.apply(0))
//  println(aSmallList.duplicateEach(3))
//  println(RNil.duplicateEach(3))

    println(aSmallList.rotate(0))
    println(aSmallList.rotate(1))
    println(aSmallList.rotate(2))
    println(aSmallList.rotate(3))
    println(aSmallList.rotate(4))
    println(aSmallList.rotate(5))
    println(aSmallList.rotate(-1))
}
