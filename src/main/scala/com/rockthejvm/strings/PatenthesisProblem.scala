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

  println(hasValidParentheses(""))
  println(hasValidParentheses("()()"))
  println(hasValidParentheses("(())"))
  println(hasValidParentheses(")("))
  println(hasValidParentheses(")"))
  println(hasValidParentheses("(("))
}
