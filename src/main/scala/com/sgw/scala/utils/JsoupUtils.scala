package com.sgw.scala.utils

import org.jsoup.nodes.{Document, Element}

import scala.util.matching.Regex

object JsoupUtils extends Loggable {
  def getElementsFrom(elem: Element, query: String): Array[Element] = elem.select(query).toArray.map {
    case element: Element => element
  }

  def getFirstElementFrom(elem: Element, query: String): Option[Element] = {
    val elements = elem.select(query)
    if (elements.isEmpty) None else Some(elements.first)
  }

  def getFirstElementTextFrom(elem: Element, query: String): Option[String] =
    getFirstElementFrom(elem, query).map(_.text().trim)

  def getInt(doc: Document, cssSelector: String): Int =
    doc.select(cssSelector).first.text.trim.toInt

  def remove(ch: Char, from: String): String = {
    val index = from.indexOf(ch)

    if (index == 0) {
      if (from.length == 1) {
        ""
      } else {
        remove(ch, from.substring(1))
      }
    } else if (index == from.length - 1) {
      from.substring(0, from.length - 1)
    } else if (index > 0) {
      from.substring(0, index) + remove(ch, from.substring(index + 1))
    } else {
      from
    }
  }

  def toDouble(value: String): Double = {
    val trimmedValue = value.trim
    remove(',', if (trimmedValue.startsWith("$")) trimmedValue.substring(1) else trimmedValue).toDouble
  }

  def getDoubleFrom(elem: Element, query: String, toDouble: (String) => Double = toDouble): Option[Double] =
    getFirstElementTextFrom(elem, query).map(value => toDouble(value))

  def extractFromElementAttr(attr: String)(elem: Element) = elem.attr(attr)

  def extractFromElementText(elem: Element): String = elem.text()

  def extractFromElementText(regex: Regex)(elem: Element): String = elem.text() match {
    case regex(value) => value
  }
}

