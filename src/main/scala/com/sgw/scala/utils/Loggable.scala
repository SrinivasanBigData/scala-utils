package com.sgw.scala.utils

import org.slf4j.LoggerFactory

/**
 * A mix-in trait used to give a class the ability to log stuff using simple function.
 * To use it, do this
 * <pre>
 *   class MyClass extends SomethingElse with Loggable {
 *     ...
 *     debug("Log some debug information")
 *     info("Log some information")
 *     warn("Log a warning")
 *     error("Log an error")
 *     ...
 *   }
 * </pre>
 */
trait Loggable {
  self =>

  private val LOG = LoggerFactory.getLogger(self.getClass)

  def isDebugEnabled = LOG.isDebugEnabled

  def debug[T](msg: => T): Unit = if (LOG.isDebugEnabled) LOG.debug(msg.toString)
  def info [T](msg: => T): Unit = if (LOG.isInfoEnabled)  LOG.info(msg.toString)
  def warn [T](msg: => T): Unit = if (LOG.isWarnEnabled)  LOG.warn(msg.toString)
  def error[T](msg: => T): Unit = if (LOG.isErrorEnabled) LOG.error(msg.toString)

  def debug[T](msg: => T, t: Throwable): Unit = if (LOG.isDebugEnabled) LOG.debug(msg.toString, t)
  def info [T](msg: => T, t: Throwable): Unit = if (LOG.isInfoEnabled)  LOG.info(msg.toString, t)
  def warn [T](msg: => T, t: Throwable): Unit = if (LOG.isWarnEnabled)  LOG.warn(msg.toString, t)
  def error[T](msg: => T, t: Throwable): Unit = if (LOG.isErrorEnabled) LOG.error(msg.toString, t)
}
