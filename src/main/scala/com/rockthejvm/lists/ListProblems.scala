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
  val aSmallList_2 = 4 :: 5 :: RNil // RNil.::(3).::(2).::(1)
  println(aSmallList)
  println(aSmallList ++ aSmallList_2)
  println(aSmallList ++ RNil)
  println(RNil ++ aSmallList)
  println(RNil ++ RNil)

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
}
