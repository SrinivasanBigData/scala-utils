package com.sgw.scala.utils

import java.io.File
import java.net.URL
import java.util.concurrent.TimeUnit

import scala.io.Source

/**
 * Interface to system-property configuration.
 */
object SystemPropertyConfig {
  /**
   * Returns an optional string for the specified system property key.
   *
   * @param key the system property key
   */
  def optional(key: String): Option[String] = Option(System.getProperty(key, null))

  // methods that return optional things
  def getLong(key: String): Option[Long]             = optional(key).map(_.toLong)
  def getInt(key: String): Option[Int]               = optional(key).map(_.toInt)
  def getString(key: String): Option[String]         = optional(key)
  def getStrings(key: String): Option[Seq[String]]   = optional(key).map(_.split(",").map(_.trim))
  def getStringSet(key: String): Option[Set[String]] = getStrings(key).map(_.toSet)
  def getTimeUnit(key: String): Option[TimeUnit]     = optional(key).map(TimeUnit.valueOf)
  def getTime(timeKey: String, timeUnitKey: String): Option[Time] =
    optional(timeKey).map(value => Time(value.toLong, getTimeUnit(timeUnitKey).getOrElse(TimeUnit.MILLISECONDS)))
  def getFile(key: String): Option[File]             = optional(key).map(new File(_))
  def getURI(key: String): Option[URI]               = optional(key).map(URI(_))
  def getURL(key: String): Option[URL]               = getURI(key).map(_.toURL)
  def getSource(key: String): Option[Source]         = getURL(key).map(Source.fromURL)
  def getJSON(key: String): Option[JSON] = getSource(key)
    .map(source => source.getLines().mkString("\n"))
    .flatMap(jsonStr => JSON.parseFull(jsonStr))
  def getResource(key: String, cls: Class[_]): Option[URL] =
    getURI(key).flatMap(uri => if (uri.isAbsolute) Some(uri.toURL) else getString(key).map(cls.getResource))
  def getSource(key: String, cls: Class[_]): Option[Source] = getResource(key, cls).map(Source.fromURL)
  def getJSON(key: String, cls: Class[_]): Option[JSON] = getSource(key, cls)
    .map(source => source.getLines().mkString("\n"))
    .flatMap(jsonStr => JSON.parseFull(jsonStr))
  def getJSONMap(key: String, cls: Class[_]): Option[Map[String, _]] = getJSON(key, cls).map(json => json.asMap)
  def getClassByClassName(key: String): Option[Class[_]] = getString(key).map(className => ClassUtils.getClass(className))
  def getObjectClassByClassName(key: String): Option[Class[_]] = getString(key).map(className => ClassUtils.getObjectClass(className))
  def getObjectByClassName(key: String): Option[AnyRef] = getString(key).map(className => ClassUtils.getObjectByClassName[Object](className))

  def except(key: String) = throw new RuntimeException(s"You must specify the -D$key system property.")

  // methods that return a default thing if the actual thing isn't defined
  def getLong(key: String, default: => Long): Long       = getLong(key).getOrElse(default)
  def getInt(key: String, default: => Int): Int          = getInt(key).getOrElse(default)
  def getString(key: String, default: => String): String = getString(key).getOrElse(default)
  def getStrings(key: String, default: => Seq[String]): Seq[String]   = getStrings(key).getOrElse(default)
  def getStringSet(key: String, default: => Set[String]): Set[String] = getStringSet(key).getOrElse(default)
  def getTimeUnit(key: String, default: => TimeUnit): TimeUnit = getTimeUnit(key).getOrElse(default)
  def getTime(timeKey: String, timeUnitKey: String, default: => Time): Time = getTime(timeKey, timeUnitKey).getOrElse(default)
  def getFile(key: String, default: => File): File       = getFile(key).getOrElse(default)
  def getURI(key: String, default: => URI): URI          = getURI(key).getOrElse(default)
  def getURL(key: String, default: => URL): URL          = getURL(key).getOrElse(default)
  def getSource(key: String, default: => Source): Source = getSource(key).getOrElse(default)
  def getJSON(key: String, default: => JSON): JSON       = getJSON(key).getOrElse(default)
  def getResource(key: String, cls: Class[_], default: => URL): URL     = getResource(key, cls).getOrElse(default)
  def getSource(key: String, cls: Class[_], default: => Source): Source = getSource(key, cls).getOrElse(default)
  def getJSON(key: String, cls: Class[_], default: => JSON): JSON       = getJSON(key, cls).getOrElse(default)
  def getJSONMap(key: String, cls: Class[_], default: => Map[String, _]): Map[String, _] = getJSONMap(key, cls).getOrElse(default)
  def getClassByClassName(key: String, default: => Class[_]): Class[_] = getClassByClassName(key).getOrElse(default)
  def getObjectClassByClassName(key: String, default: => Class[_]): Class[_] = getObjectClassByClassName(key).getOrElse(default)
  def getObjectByClassName(key: String, default: => AnyRef): AnyRef = getObjectByClassName(key).getOrElse(default)
}

