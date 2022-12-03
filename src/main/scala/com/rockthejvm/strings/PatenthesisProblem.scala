package com.rockthejvm.strings

import scala.annotation.tailrec

object PatenthesisProblem extends App {

  /**
   * "()" => true
   * "()()" => true
   * "(())" => true
   * ")(" => false
   */
  // TODO Valid Parantheses
  //  Initial time to solve:: 7:32
  //  Complexity: O(n)
  //  Final time: 17:52
  def hasValidParentheses(string: String): Boolean = {
    @tailrec
    def checkTailrec(remaining: String, opened: Int): Boolean = {
      if (remaining.isEmpty) opened == 0
      else {
        remaining.head match
          case '(' => checkTailrec(remaining.tail, opened + 1)
          case ')' =>
            if (opened == 0) false
            else checkTailrec(remaining.tail, opened - 1)
      }
    }

    checkTailrec(string, 0)
  }

  /*
    n = 1 => List("()")
    n = 2 => List("(())", "()()")
    n = 3 => List("()()()", "()(())", "(())()", "((()))", "(()())")
  */
  // TODO Generate All Parentheses
  //  Initial time to solve:: 21:30
  //  Complexity: O(3^n)
  //  Final time:
//  def generateAllValidParentheses(n: Int): List[String] = {
//    @tailrec
//    def generateTailrec(k: Int, acc: List[String]): List[String] = {
//      if (k <= 0) acc
//      else {
//        val variations: List[String] = acc.flatMap((s: String) => List(s"()$s", s"($s)", s"$s()")).toSet.toList // legit? :D
//        generateTailrec(k-1, variations)
//      }
//    }
//
//    generateTailrec(n, List(""))
//  }

  def generateAllValidParentheses(n: Int): List[String] = {
    @tailrec
    def generateTailrec(k: Int, acc: Set[String]): Set[String] = {
      if (k <= 0) acc
      else {
        val variations: Set[String] = acc.flatMap((s: String) => Set(s"()$s", s"($s)", s"$s()"))
        generateTailrec(k-1, variations)
      }
    }

    generateTailrec(n, Set("")).toList
  }

//  println(hasValidParentheses(""))
//  println(hasValidParentheses("()()"))
//  println(hasValidParentheses("(())"))
//  println(hasValidParentheses(")("))
//  println(hasValidParentheses(")"))
//  println(hasValidParentheses("(("))


  println(generateAllValidParentheses(-1))
  println(generateAllValidParentheses(0))
  println(generateAllValidParentheses(1))
  println(generateAllValidParentheses(2))
  println(generateAllValidParentheses(3))
}
