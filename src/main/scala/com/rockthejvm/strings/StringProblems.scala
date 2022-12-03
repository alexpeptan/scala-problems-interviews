package com.rockthejvm.strings

import scala.annotation.tailrec

object StringProblems extends App {

  // TODO Count Chars
  // Initial time to solve:: 21:23
  // Lessons:
  // - map vs immutable map
  // - map api
  // Complexity: O(length(s))
  // Final time: 24:45 (one extra problem :D)
  def countCharacters(s: String): Map[Char, Int] = {
    @tailrec
    def countCharsTailrec(remaining: String, counts: Map[Char, Int]): Map[Char, Int] = {
      if (remaining.isEmpty) counts
      else {
        val currentChar = remaining.charAt(0) // or remaining.head
        // counts.updated(currentChar, 1 + counts.getOrElse(currentChar, 0))
        countCharsTailrec(remaining.substring(1), // or remaining.tail
          // counts.updated(currentChar, 1 + counts.getOrElse(currentChar, 0)))
        // or
          counts + (currentChar ->  (1 + counts.getOrElse(currentChar, 0))))
      }
    }

    countCharsTailrec(s, Map[Char, Int]())
  }

  println(countCharacters("yioyoyoyoyo hahaha xoxo"))
  println(countCharacters(""))
}
