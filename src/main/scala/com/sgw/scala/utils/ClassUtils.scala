package com.sgw.scala.utils

/**
 * Utilities for manipulating classes.
 */
object ClassUtils {
  def getObjectByClassName[T](className: String): T = {
    val cls = getObjectClass(className)
    cls.getField("MODULE$").get(cls).asInstanceOf[T]
  }

  def getObjectClass(classname: String): Class[_] = getClass(classname + "$")

  def getClass(className: String): Class[_] = Class.forName(className)
}
