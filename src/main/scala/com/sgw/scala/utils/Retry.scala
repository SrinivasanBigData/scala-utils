package com.sgw.scala.utils

import scala.annotation.tailrec
import scala.util.{Failure, Try}

/**
 * A wrapper similar to Scala's "scala.util.Try" that will retry a specified function for a specified
 * number of times with a specified delay between each try.
 */
case class Retry(maxTries: Int, delay: Time = Time.ZERO) extends Loggable {
  def apply[T](r: => T): Try[T] = apply(0, r, Failure[T](new RuntimeException("Max tries (" + maxTries + ") exceeded.")))

  @tailrec
  private def apply[T](tries: Int, r: => T, lastTry: Try[T]): Try[T] = {
    if (lastTry.isSuccess || tries >= maxTries) return lastTry

    if (tries > 0) {
      val ex = lastTry match {
        case Failure(e) => e
        case _ => new RuntimeException("This should never happen.")
      }

      warn("Failed on try #" + tries + "!", ex)

      if (delay.isPositive) {
        warn("Sleeping for " + delay.toSimplifiedUnits + " ...")
        delay.sleep()
      }

      warn("Retrying...")
    }

    apply(tries + 1, r, Try(r))
  }
}
