package com.sgw.scala.utils

import java.text.{DateFormatSymbols, SimpleDateFormat}
import java.util.concurrent.TimeUnit._
import java.util.concurrent._
import java.util.{Date, Locale}

import scala.concurrent.duration.Duration
import scala.util.Try

trait TimeConversions extends Any {
  protected def timeIn(unit: TimeUnit): Time

  def nanoseconds  = timeIn(NANOSECONDS)
  def nanos        = nanoseconds
  def nanosecond   = nanoseconds
  def nano         = nanoseconds

  def microseconds = timeIn(MICROSECONDS)
  def micros       = microseconds
  def microsecond  = microseconds
  def micro        = microseconds

  def milliseconds = timeIn(MILLISECONDS)
  def millis       = milliseconds
  def millisecond  = milliseconds
  def milli        = milliseconds

  def seconds      = timeIn(SECONDS)
  def second       = seconds

  def minutes      = timeIn(MINUTES)
  def minute       = minutes

  def hours        = timeIn(HOURS)
  def hour         = hours

  def days         = timeIn(DAYS)
  def day          = days
}

/**
 * A factory for Time objects.
 */
object Time {
  /**
   * An implicit used to convert an integer to a time as in "1 second" or "5 minutes".
   *
   * @param n the value to convert
   */
  implicit final class TimeInt(val n: Int) extends AnyVal with TimeConversions {
    protected def timeIn(unit: TimeUnit): Time = Time(n, unit)
  }

  /**
   * An implicit used to convert a long to a time as in "1 second" or "5 minutes".
   *
   * @param n the value to convert
   */
  implicit final class TimeLong(val n: Long) extends AnyVal with TimeConversions {
    protected def timeIn(unit: TimeUnit): Time = Time(n, unit)
  }

  private lazy val YearMonthDayDateFormat = new SimpleDateFormat("YYYY-MM-dd", new DateFormatSymbols(Locale.US))
  private lazy val ISO8601DateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")

  private lazy val SchedulerPoolSize = 10

  // This pool contains daemon threads only, so the app can shutdown (without having to remember to call
  // "shutdown" on the thread pool) even if the pool contains threads, scheduled or not.
  private lazy val Scheduler = Executors.newScheduledThreadPool(SchedulerPoolSize, new ThreadFactory() {
    @Override
    def newThread(runnable: Runnable): Thread = {
      val thread = Executors.defaultThreadFactory().newThread(runnable)
      thread.setDaemon(true) // make'm daemons
      thread
    }
  })

  lazy val ZERO     = millis(0L)
  lazy val INFINITE = millis(Long.MaxValue)
  lazy val INVALID  = INFINITE

  val MILLIS_PER_SECOND = 1000
  val MILLIS_PER_MINUTE = MILLIS_PER_SECOND * 60
  val MILLIS_PER_HOUR   = MILLIS_PER_MINUTE * 60
  val MILLIS_PER_DAY    = MILLIS_PER_HOUR * 24

  def formatAsDate(date: Date): String = YearMonthDayDateFormat.synchronized {
    YearMonthDayDateFormat.format(date)
  }

  def formatAsDate(time: Time): String = formatAsDate(time.toDate)

  def parseAsYearMonthDay(date: String): Time = YearMonthDayDateFormat.synchronized {
    Time(YearMonthDayDateFormat.parse(date))
  }

  def formatAsISO8601(date: Date): String = ISO8601DateFormat.synchronized {
    ISO8601DateFormat.format(date)
  }

  def formatAsISO8601(time: Time): String = formatAsISO8601(time.toDate)

  def parseAsISO8601(date: String): Time = ISO8601DateFormat.synchronized {
    Time(ISO8601DateFormat.parse(date))
  }

  /**
   * Functions used to create Time object with various TimeUnits.
   * @param value the tiem value
   * @return a Time object
   */
  def nanos(value: Long)   = Time(value, NANOSECONDS)
  def micros(value: Long)  = Time(value, MICROSECONDS)
  def millis(value: Long)  = Time(value, MILLISECONDS)
  def seconds(value: Long) = Time(value, SECONDS)
  def minutes(value: Long) = Time(value, MINUTES)
  def hours(value: Long)   = Time(value, HOURS)
  def days(value: Long)    = Time(value, DAYS)
  def weeks(value: Long)   = days(value * 7)

  /**
   * Returns the current time as a Time object.
   *
   * @return the current time as a Time object.
   */
  def now = millis(System.currentTimeMillis())

  /**
   * Returns a random time object between the specified fromTime (inclusive) and toTime (exclusive).
   *
   * @param fromTime the minimum time of the random time (inclusive)
   * @param toTime the maximum time of the random time (exclusive)
   *
   * @return a random time object between the specified fromTime (inclusive) and toTime (exclusive).
   */
  def random(fromTime: Time, toTime: Time): Time = fromTime + (toTime - fromTime).random

  def wrapInRunnable(f: () => Unit) = new Runnable { def run(): Unit = f() }

  /**
   * Creates and executes a one-shot action that becomes enabled
   * after the given delay.
   *
   * @param f the function to execute
   * @param delay the time from now to delay execution

   * @return a ScheduledFuture representing pending completion of
   *         the task and whose <tt>get()</tt> method will return
   *         <tt>null</tt> upon completion
   * @throws RejectedExecutionException if the task cannot be
   *                                    scheduled for execution
   * @throws NullPointerException if command is null
   */
  def schedule(f: () => Unit, delay: Time): ScheduledFuture[_] =
    Scheduler.schedule(wrapInRunnable(f), delay.time, delay.units)

  /**
   * Creates and executes a periodic action that becomes enabled first
   * after the given initial delay, and subsequently with the given
   * period; that is executions will commence after
   * <tt>initialDelay</tt> then <tt>initialDelay+period</tt>, then
   * <tt>initialDelay + 2 * period</tt>, and so on.
   * If any execution of the task
   * encounters an exception, subsequent executions are suppressed.
   * Otherwise, the task will only terminate via cancellation or
   * termination of the executor.  If any execution of this task
   * takes longer than its period, then subsequent executions
   * may start late, but will not concurrently execute.
   *
   * @param f the function to execute
   * @param initialDelay the time to delay first execution
   * @param period the period between successive executions
   * @return a ScheduledFuture representing pending completion of
   *         the task, and whose <tt>get()</tt> method will throw an
   *         exception upon cancellation
   * @throws RejectedExecutionException if the task cannot be
   *                                    scheduled for execution
   * @throws NullPointerException if command is null
   * @throws IllegalArgumentException if period less than or equal to zero
   */
  def scheduleAtFixedRate(f: () => Unit, initialDelay: Time, period: Time): ScheduledFuture[_] =
    Scheduler.scheduleAtFixedRate(wrapInRunnable(f), initialDelay.millis, period.millis, TimeUnit.MILLISECONDS)

  /**
   * Creates and executes a periodic action that becomes enabled first
   * after the given initial delay, and subsequently with the
   * given delay between the termination of one execution and the
   * commencement of the next.  If any execution of the task
   * encounters an exception, subsequent executions are suppressed.
   * Otherwise, the task will only terminate via cancellation or
   * termination of the executor.
   *
   * @param f the function to execute
   * @param initialDelay the time to delay first execution
   * @param delay the delay between the termination of one
   *              execution and the commencement of the next
   * @return a ScheduledFuture representing pending completion of
   *         the task, and whose <tt>get()</tt> method will throw an
   *         exception upon cancellation
   * @throws RejectedExecutionException if the task cannot be
   *                                    scheduled for execution
   * @throws NullPointerException if command is null
   * @throws IllegalArgumentException if delay less than or equal to zero
   */
  def scheduleWithFixedDelay(f: () => Unit, initialDelay: Time, delay: Time): ScheduledFuture[_] =
    Scheduler.scheduleWithFixedDelay(wrapInRunnable(f), initialDelay.millis, delay.millis, TimeUnit.MILLISECONDS)

  /**
   * Converts a java Date to a Time.
   * @param date the date
   * @return the Time represented by the millis of the specified date
   */
  def apply(date: Date): Time = millis(date.getTime)
}

/**
 * An object representing a point in time.
 *
 * @param time the time value
 * @param units the time units
 */
case class Time(time: Long, units: TimeUnit = MILLISECONDS) extends Comparable[Time] {
  def isNanos   = units == NANOSECONDS
  def isMicros  = units == MICROSECONDS
  def isMillis  = units == MILLISECONDS
  def isSeconds = units == SECONDS
  def isMinutes = units == MINUTES
  def isHours   = units == HOURS
  def isDays    = units == DAYS

  def toNanos   = convertTo(NANOSECONDS)
  def toMicros  = convertTo(MICROSECONDS)
  def toMillis  = convertTo(MILLISECONDS)
  def toSeconds = convertTo(SECONDS)
  def toMinutes = convertTo(MINUTES)
  def toHours   = convertTo(HOURS)
  def toDays    = convertTo(DAYS)
  def toDate    = new Date(millis)

  def nanos   = toNanos.time
  def micros  = toMicros.time
  def millis  = toMillis.time
  def seconds = toSeconds.time
  def minutes = toMinutes.time
  def hours   = toHours.time
  def days    = toDays.time

  def random: Time = this * scala.math.random

  def +(rhs: Time) = Time.millis(millis + rhs.millis)
  def -(rhs: Time) = Time.millis(millis - rhs.millis)
  def /(rhs: Time) = Time.millis(millis / rhs.millis)
  def *(rhs: Time) = Time.millis(millis * rhs.millis)

  def /(value: Double) = Time.millis((millis / value).toLong)
  def *(value: Double) = Time.millis((millis * value).toLong)

  def +(value: Long) = Time.millis(millis + value)
  def -(value: Long) = Time.millis(millis - value)
  def /(value: Long) = Time.millis(millis / value)
  def *(value: Long) = Time.millis(millis * value)

  def isZero     = time == 0
  def isNotZero  = !isZero
  def isPositive = time > 0
  def isPositiveOrZero = time >= 0
  def isNegative = time < 0
  def isNegativeOrZero = time <= 0
  def isInfinite = time == Long.MaxValue
  def isNotInfinite = !isInfinite
  def isInvalid = this == Time.INVALID

  def ==(ms: Long) = millis == ms
  def < (ms: Long) = millis <  ms
  def > (ms: Long) = millis >  ms
  def <=(ms: Long) = millis <= ms
  def >=(ms: Long) = millis >= ms

  def ==(rhs: Time) = millis == rhs.millis
  def < (rhs: Time) = millis <  rhs.millis
  def > (rhs: Time) = millis >  rhs.millis
  def <=(rhs: Time) = millis <= rhs.millis
  def >=(rhs: Time) = millis >= rhs.millis

  def compareTo(rhs: Time) = millis.compareTo(rhs.millis)

  def min(rhs: Time): Time = if (units == rhs.units) {
    if (time < rhs.time) {
      this
    } else {
      rhs
    }
  } else {
    min(rhs.convertTo(units))
  }

  def max(rhs: Time): Time = if (units == rhs.units) {
    if (time > rhs.time) {
      this
    } else {
      rhs
    }
  } else {
    max(rhs.convertTo(units))
  }

  def toSimplifiedUnits: Time = {
    val ms = millis

    if (ms == 0) toSeconds
    else if (ms % Time.MILLIS_PER_DAY    == 0) toDays
    else if (ms % Time.MILLIS_PER_HOUR   == 0) toHours
    else if (ms % Time.MILLIS_PER_MINUTE == 0) toMinutes
    else if (ms % Time.MILLIS_PER_SECOND == 0) toSeconds
    else toMillis
  }

  def sleep(): Try[Unit] = Try { if (isPositive) Thread.sleep(millis) }

  def convertTo(toUnit: TimeUnit): Time = if (units == toUnit) this else Time(toUnit.convert(time, units), toUnit)

  def toDouble(toUnit: TimeUnit): Double = toUnit match {
    case TimeUnit.NANOSECONDS => millis.toDouble * 1000000
    case TimeUnit.MICROSECONDS => millis.toDouble * 1000
    case TimeUnit.MILLISECONDS => millis.toDouble
    case TimeUnit.SECONDS => millis.toDouble / Time.MILLIS_PER_SECOND
    case TimeUnit.MINUTES => millis.toDouble / Time.MILLIS_PER_MINUTE
    case TimeUnit.HOURS => millis.toDouble / Time.MILLIS_PER_HOUR
    case TimeUnit.DAYS => millis.toDouble / Time.MILLIS_PER_DAY
  }

  def toDuration = Duration(time, units)

  def roundUp: Time = {
    val ms = toMillis.time

    if (ms < Time.MILLIS_PER_SECOND)      toMillis
    else if (ms < Time.MILLIS_PER_MINUTE) toSeconds
    else if (ms < Time.MILLIS_PER_HOUR)   toMinutes
    else if (ms < Time.MILLIS_PER_DAY)    toHours
    else toDays
  }

  def toMap = Map("time" -> time, "units" -> units)

  def delta(to: Time) = if (this.isInvalid || to.isInvalid) Time.INVALID else { to - this }

  def age = delta(Time.now)

  lazy val toDateString = Time.formatAsDate(this)

  lazy val toISO8601DateString = Time.formatAsISO8601(this)

  def singularUnitsString = pluralUnitsString.substring(0, pluralUnitsString.length - 1)

  def pluralUnitsString = units.toString.toLowerCase

  def unitsString = if (time == 1L) singularUnitsString else pluralUnitsString

  def toStringImpl = time.toString + " " + unitsString

  override def toString: String = toSimplifiedUnits.toStringImpl

  def toString(units: TimeUnit): String = convertTo(units).toString
}

