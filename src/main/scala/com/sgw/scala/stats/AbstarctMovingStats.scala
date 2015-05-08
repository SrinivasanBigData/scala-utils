package com.sgw.scala.stats

import com.sgw.scala.utils.Time

/**
 * A base class for objects that maintain a moving window of statistical information.
 *
 * @param windowSize the size of the moving window (number of samples)
 * @param total the total of all the values in the window
 * @param values the moving stat's current total
 */
abstract class AbstractMovingStats(val windowSize: Int = 100, total: Long, values: Vector[Long] = Vector[Long]()) {
  type NextState >: this.type

  val PERCENTILES = List(0, 1, 5, 25, 50, 75, 95, 99, 100)
  val QUARTILES   = List(0, 25, 60, 75, 100)

  def size  = values.size // number of items in the window
  def rank(p: Int) = (p / 100.0 * (size - 0.5)).toInt // see wikipedia, used to calc. percentiles
  def percentile(p: Int) = values.sorted.apply(rank(p))
  def ranks(ps: Seq[Int] = PERCENTILES) = ps.map(p => rank(p))
  def percentiles(ps: Seq[Int] = PERCENTILES): Seq[Long] = {
    val sorted = values.sorted
    ranks(ps).map(i => if (i >= 0 && i < size) sorted(i) else 0)
  }
  def quartiles = percentiles(QUARTILES)
  def min = percentile(0)
  def max = percentile(100)
  def median = percentile(50)
  def q1 = percentile(25)
  def q2 = median
  def q3 = percentile(75)

  def avg = total.toDouble / values.size
  def variance = {
    val mean = avg
    values.map(x => x - mean).map(diff => diff * diff).sum / values.size
  }
  def stddev = scala.math.sqrt(variance)

  def isValid = size > 0
  def isFull  = size >= windowSize
  def isEmpty = size == 0

  def add(time: Time): NextState = add(time.millis)

  def add(newValue: Long): NextState = {
    val (newTotal, newBuffer) = if (isFull) {
      (total - values.head + newValue, values.tail :+ newValue)
    } else {
      (total + newValue, values :+ newValue)
    }

    nextState(newTotal, newBuffer)
  }

  def apply(newValue: Long): NextState = add(newValue)

  protected def nextState(newTotal: Long, newBuffer: Vector[Long]): NextState

  override def toString = toStringBuilder(new StringBuilder, "").toString()

  def toStringBuilder(builder: StringBuilder, prefix: String = "  "): StringBuilder = {
    builder.append(prefix).append("Moving Average     : %10d\n".format(avg.toLong))
    builder.append(prefix).append("Moving Percentiles : ").append(percentiles().mkString(",")).append("\n")
    builder.append(prefix).append("Moving Std. Dev.   : %10d".format(stddev.toLong))
  }
}
