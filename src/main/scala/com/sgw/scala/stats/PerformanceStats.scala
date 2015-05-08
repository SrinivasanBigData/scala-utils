package com.sgw.scala.stats

/**
 * A type of moving stats function used to capture performance stats.
 *
 * @param windowSize the max number of samples in the moving window
 */
case class PerformanceStats(override val windowSize: Int = 100, newTotal: Long = 0, newBuffer: Vector[Long] = Vector[Long]())
  extends AbstractMovingStats(windowSize, newTotal, newBuffer) {
  type NextState = PerformanceStats

  protected def nextState(newTotal: Long, newBuffer: Vector[Long]): NextState =
    PerformanceStats(windowSize, newTotal, newBuffer)

  def apply[R](code: => R): (R, PerformanceStats) = {
    val startTime = System.currentTimeMillis()
    val result = code
    val endTime = System.currentTimeMillis()
    (result, add(endTime - startTime))
  }
}
