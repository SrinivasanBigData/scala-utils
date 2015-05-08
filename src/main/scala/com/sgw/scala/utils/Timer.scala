package com.sgw.scala.utils

/**
 * A simple class used to time how long it takes to execute a specified function.
 */
object Timer {
  def apply[R](code: => R): (Time, R) = {
    val startTime = Time.now
    val result = code
    (startTime.age, result)
  }
}
