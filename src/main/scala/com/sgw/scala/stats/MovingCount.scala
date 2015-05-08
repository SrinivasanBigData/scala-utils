package com.sgw.scala.stats

import com.sgw.scala.utils.Time

/**
 * Contains count of events that occurred within a moving window of time.
 * Applying a new event returns a new instance of this class.
 *
 * @param timeWindow the time window
 * @param events a time-series of event counts
 * @param count the total count of the number of events within the time window
 */
case class MovingCount(
  timeWindow: Time = Time.minutes(1),
  events: Vector[(Time, Int)] = Vector[(Time, Int)](),
  count: Int = 0
) {
  private def shouldExpire(event: (Time, Int), now: Time): Boolean = event match {
    case (eventTime, _) => shouldExpire(eventTime, now)
  }

  private def shouldExpire(eventTime: Time, now: Time): Boolean = eventTime <= now - timeWindow

  /**
   * Returns a new MovingCount for the specified event.
   *
   * @param now the new MovingCount's most recent time
   * @param cnt the number of events to record at the specified time
   */
  def event(now: Time = Time.now, cnt: Int = 1): MovingCount = {
    val newCount = count - events.takeWhile(shouldExpire(_, now)).map {
      case (eventTime, eventCount) => eventCount
    }.sum + cnt

    val newBuffer = events.dropWhile(shouldExpire(_, now)) :+ (now, cnt)

    MovingCount(timeWindow, newBuffer, newCount)
  }

  /**
   * Returns a new MovingCount for the specified time without adding a new event.
   *
   * @param now the new time
   */
  def asOf(now: Time = Time.now): MovingCount = {
    val newCount = count - events.takeWhile(shouldExpire(_, now)).map {
      case (eventTime, eventCount) => eventCount
    }.sum

    val newBuffer = events.dropWhile(shouldExpire(_, now))

    MovingCount(timeWindow, newBuffer, newCount)
  }

  override def toString: String = s"$count/$timeWindow)"
}
