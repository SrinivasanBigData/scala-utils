package com.sgw.scala.utils

import org.apache.http.entity.ContentType
import org.jsoup.nodes.{Document, Element}

import scala.collection.JavaConversions._
import scala.collection.SeqView

/**
 * Utils used to create and manipulate URIs.
 */
object URIUtils extends Loggable {
  def contentTypeEquals(ct1: ContentType, ct2: ContentType) = ct1.getMimeType.equals(ct2.getMimeType) && ct1.getCharset.name.equals(ct2.getCharset.name)

  // simple for now
  def percentDecode(str: String) = str
    .replace("%2D", "-")
    .replace("%2d", "-")
    .replace("%3D", "=")
    .replace("%3d", "=")
    .replace("%7C", "|")
    .replace("%7c", "|")

  def ampDecode(str: String) = str.replace("&amp;", "&").replace("&AMP;", "&")

  def decode(str: String) = ampDecode(percentDecode(str))

  /**
   * Creates a sequence of URIs from a sequence of URI strings filtering out empty strings a javascript URIs.
   *
   * @param uriStrs a sequence of URI strings to be converted to URIs
   *
   * @return a sequence of URIs
   */
  def createURIs(uriStrs: SeqView[String, Seq[_]]) =
    uriStrs.filter(_ != null)
      .map(_.trim)
      .filter(_ != "")
      .filter(!_.startsWith("javascript"))
      .map(uriStr => try {
      URI(uriStr, ignoreFragment = true)
    } catch {
      case ex: Exception => {
        error("Failed to parse uri: " + uriStr + ". " + ex.getMessage + ". Skipping.", ex)
        URI.BadURI
      }
    })
      .filter(uri => uri != URI.BadURI)

  def extractURIsFromHTML(
    baseURI: URI,
    doc: Document,
    cssQuery: String = "a[href]",
    elemURIExtractFunc: (Element) => String = JsoupUtils.extractFromElementAttr("href")
  ): SeqView[URI, Seq[_]] = {
    // see if the document contains a <base href="..."/> tag
    val baseURIs: List[String] = doc
      .select("base[href]")
      .toArray
      .map { case elem: Element => JsoupUtils.extractFromElementAttr("href")(elem) }
      .toList

    val actualBaseURI = if (baseURIs.isEmpty) baseURI else URI(baseURIs(0), ignoreFragment = true)

    val uris = doc
      .select(cssQuery)
      .view
      .map { case elem: Element => elemURIExtractFunc(elem) }

    createURIs(uris)
      .map(uri => {
      try {
        if (uri.isAbsolute) {
          uri
        } else {
          URI(actualBaseURI.resolve(uri), ignoreFragment = true)
        }
      } catch {
        case ex: Exception => {
          error("Failed to resolve '" + uri + "' to '" + baseURI + "'.", ex)
          throw ex
        }
      }
    })
  }

  def extractURIsFromRobotsDotText(baseURI: URI, page: String): SeqView[URI, Seq[_]] = page
    .split("\n")
    .toList
    .view
    .filter(line => line.toLowerCase.startsWith("sitemap:"))
    .map(line => line.substring("sitemap:".length).trim)
    .map(uriStr => baseURI.resolve(URI(uriStr, ignoreFragment = true)))

  def extractURIsFromXMLSiteMap(baseURI: URI, doc: Document): SeqView[URI, Seq[_]] =
    extractURIsFromHTML(baseURI, doc, "loc", JsoupUtils.extractFromElementText)
}

