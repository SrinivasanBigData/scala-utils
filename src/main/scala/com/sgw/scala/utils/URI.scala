package com.sgw.scala.utils

import java.net.URL

import scala.annotation.tailrec
import scala.io.Source

/**
 * A factory for URIs
 */
object URI extends Loggable {
  lazy val defaultPorts: Map[String, Int] =
    Map(
      "ftp" -> 21,
      "ssh" -> 22,
      "telnet" -> 23,
      "smtp" -> 25,
      "domain" -> 53,
      "tftp" -> 69,
      "http" -> 80,
      "pop3" -> 110,
      "nntp" -> 119,
      "imap" -> 143,
      "snmp" -> 161,
      "ldap" -> 389,
      "https" -> 443,
      "imaps" -> 993,
      "nfs" -> 2049
    ).withDefaultValue(-1)

  lazy val BadURI = apply("BadURI")
  lazy val Empty = apply("")
  lazy val UnknownURI = apply("UnknownURI")

  /**
   * Creates a URI from a URI string.
   *
   * @param uriStr the URI string
   * @return a URI
   */
  def apply(uriStr: String): URI = apply(uriStr, ignoreFragment = true)
  def apply(uriStr: String, ignoreFragment: Boolean): URI = parse(uriStr, ignoreFragment)

  /**
   * Creates a copy of the specified URI.
   * @param uri the URI
   * @return a copy of the specified URI
   */
  def apply(uri: URI): URI = apply(uri, ignoreFragment = true)
  def apply(uri: URI, ignoreFragment: Boolean): URI = apply(
    uri.scheme,
    uri.userInfo,
    uri.host,
    uri.port,
    uri.path,
    uri.query,
    if (ignoreFragment) None else uri.fragment
  )

  /**
   * Creates a URI from a set of optional URI parts. The resulting URI is normalized.
   *
   * @param scheme an optional URI scheme (e.g. http, ftp, s3)
   * @param userInfo an optional user-info (e.g. foobar)
   * @param host an optional host (e.g. www.google.com)
   * @param port an optional port (e.g. 8080)
   * @param path an optional path (e.g. /foo/bar/baz)
   * @param query an optional query string (e.g. foo=1&bar="baz")
   * @param fragment an optional fragment (e.g. foobar)
   *
   * @return a URI
   */
  def apply(
             scheme: Option[String] = None,
             userInfo: Option[String] = None,
             host: Option[String] = None,
             port: Option[Int] = None,
             path: Option[String] = None,
             query: Option[String] = None,
             fragment: Option[String] = None
             ): URI = new URI(
    normalizeScheme(scheme),
    userInfo,
    normalizeHost(host),
    normalizePort(normalizeScheme(scheme), port),
    normalizePath(path),
    normalizeQuery(query),
    fragment
  )

  private def apply(
                     scheme: Option[String],
                     authority: Authority,
                     path: Option[String],
                     query: Option[String],
                     fragment: Option[String]
                     ): URI = apply(
    scheme,
    authority.userInfo,
    authority.host,
    authority.port,
    path,
    query,
    fragment
  )

  private val schemeAuthorityPathQueryFragmentRegEx = """^([^:]*)://([^/]*)(/[^?]*)\?([^#]*)#(.*)$""".r
  private val schemeAuthorityPathFragmentRegEx      = """^([^:]*)://([^/]*)(/[^#]*)#(.*)$""".r
  private val schemeAuthorityQueryFragmentRegEx     = """^([^:]*)://([^?]*)\?([^#]*)#(.*)$""".r
  private val schemeAuthorityFragmentRegEx          = """^([^:]*)://([^#]*)#(.*)$""".r
  private val schemeAuthorityPathQueryRegEx         = """^([^:]*)://([^/]*)(/[^?]*)\?(.*)$""".r
  private val schemeAuthorityPathRegEx              = """^([^:]*)://([^/]*)(/.*)$""".r
  private val schemeAuthorityQueryRegEx             = """^([^:]*)://([^?]*)\?(.*)$""".r
  private val schemeAuthorityRegEx                  = """^([^:]*)://(.*)$""".r

  private def parse(uriStr: String, ignoreFragment: Boolean = true): URI = uriStr.trim match {
    case schemeAuthorityPathQueryFragmentRegEx(scheme, authority, path, query, fragment) => apply(
      Some(scheme),
      Authority.apply(authority),
      Some(path),
      Some(query),
      if (ignoreFragment) None else Some(fragment)
    )
    case schemeAuthorityPathFragmentRegEx(scheme, authority, path, fragment) => apply(
      Some(scheme),
      Authority.apply(authority),
      Some(path),
      None,
      if (ignoreFragment) None else Some(fragment)
    )
    case schemeAuthorityQueryFragmentRegEx(scheme, authority, query, fragment) => apply(
      Some(scheme),
      Authority.apply(authority),
      Some("/"),
      Some(query),
      if (ignoreFragment) None else Some(fragment)
    )
    case schemeAuthorityFragmentRegEx(scheme, authority, fragment) => apply(
      Some(scheme),
      Authority.apply(authority),
      Some("/"),
      None,
      if (ignoreFragment) None else Some(fragment)
    )
    case schemeAuthorityPathQueryRegEx(scheme, authority, path, query) => apply(
      Some(scheme),
      Authority.apply(authority),
      Some(path),
      Some(query),
      None
    )
    case schemeAuthorityPathRegEx(scheme, authority, path) => apply(
      Some(scheme),
      Authority.apply(authority),
      Some(path),
      None,
      None
    )
    case schemeAuthorityQueryRegEx(scheme, authority, query) => apply(
      Some(scheme),
      Authority.apply(authority),
      Some("/"),
      Some(query),
      None
    )
    case schemeAuthorityRegEx(scheme, authority) => apply(
      Some(scheme),
      Authority.apply(authority),
      Some("/"),
      None,
      None
    )
    case _ => parseNoScheme(uriStr, ignoreFragment)
  }

  private val pathQueryFragmentRegEx                = """^([^?]*)\?([^#]*)#(.*)$""".r
  private val pathFragmentRegEx                     = """^([^#]*)#(.*)$""".r
  private val pathQueryRegEx                        = """^([^?]*)\?(.*)$""".r
  private val queryFragmentRegEx                    = """\?([^#]*)#(.*)$""".r
  private val fragmentRegEx                         = """#(.*)$""".r
  private val queryRegEx                            = """\?(.*)$""".r
  private val pathRegEx                             = """^(.*)$""".r

  private def parseNoScheme(uriStr: String, ignoreFragment: Boolean) = uriStr match {
    case pathQueryFragmentRegEx(path, query, fragment) => apply(
      None,
      None,
      None,
      None,
      Some(path),
      Some(query),
      if (ignoreFragment) None else Some(fragment)
    )
    case pathFragmentRegEx(path, fragment) => apply(
      None,
      None,
      None,
      None,
      Some(path),
      None,
      if (ignoreFragment) None else Some(fragment)
    )
    case queryFragmentRegEx(query, fragment) => apply(
      None,
      None,
      None,
      None,
      None,
      Some(query),
      if (ignoreFragment) None else Some(fragment)
    )
    case fragmentRegEx(fragment) => apply(
      None,
      None,
      None,
      None,
      None,
      None,
      if (ignoreFragment) None else Some(fragment)
    )
    case pathQueryRegEx(path, query) => apply(
      None,
      None,
      None,
      None,
      Some(path),
      Some(query),
      None
    )
    case queryRegEx(query) => apply(
      None,
      None,
      None,
      None,
      None,
      Some(query),
      None
    )
    case pathRegEx(path) => apply(
      None,
      None,
      None,
      None,
      Some(path),
      None,
      None
    )
    case _ => apply(
      None,
      None,
      None,
      None,
      None,
      None,
      None
    )
  }

  private def normalizeScheme(scheme: Option[String]): Option[String] = if (scheme.isDefined) {
    val newScheme = scheme.get.toLowerCase
    Some(if (newScheme == "https") "http" else newScheme)
  } else {
    None
  }

  private def normalizePath(path: Option[String]): Option[String] = if (path.isDefined) {
    val newPath = replaceUselessDotDotSegmentsIn(
      removeLeadingDotDotSegmentsIn(
        replaceEmptySegmentsIn(
          replaceDotSegmentsIn(path.get.replace('\\', '/'))
        )
      )
    )
      .trim
      .replace("%7E", "~")
      .replace("%7e", "~")
      .replace(" ", "%20")

    if (newPath.isEmpty) None else Some(newPath)
  } else {
    None
  }

  private def normalizeHost(host: Option[String]): Option[String] = if (host.isDefined) Some(host.get.toLowerCase) else None

  private def normalizePort(scheme: Option[String], port: Option[Int]): Option[Int] = if (scheme.isDefined && port.isDefined && port.get == defaultPorts(scheme.get)) None else port

  private def replaceUselessDotDotSegmentsIn(path: String) = """/[^./]+/[.][.]/""".r.replaceAllIn(path, "/")

  private def replaceDotSegmentsIn(path: String) = replaceAll(path, "/./", "/")

  private def replaceEmptySegmentsIn(path: String) = replaceAll(path, "//", "/")

  //@tailrec
  //private def replaceAllRegEx(str: String, regex: Regex, replacement: String): String = if (regex.findFirstIn(str).isDefined) replaceAllRegEx(regex.replaceFirstIn(str, replacement), regex, replacement) else str

  //@tailrec
  private def replaceAll(str: String, target: String, replacement: String): String = if (str.contains(target)) replaceAll(str.replace(target, replacement), target, replacement) else str

  @tailrec
  private def removeLeadingDotDotSegmentsIn(str: String): String = if (str.startsWith("/../")) removeLeadingDotDotSegmentsIn(str.substring(3)) else str

  def normalizeQuery(query: List[(String, Any)]): String = query.sortBy(keyValuePair => keyValuePair._1).map {
    case (key, value) => encodeQueryPart(key) + "=" + encodeQueryPart(value.toString)
  }.mkString("&")

  def normalizeQuery(query: Map[String, Any]): String = normalizeQuery(query.toList)

  def normalizeQuery(query: String): String = normalizeQuery(parseQuery(cleanQuery(query), Nil))

  def normalizeQuery(query: Option[String]): Option[String] = if (query.isDefined) {
    Some(normalizeQuery(query.get))
  } else {
    None
  }

  def cleanQuery(query: String) = URIUtils.decode(query)

  private def encodeQueryPart(part: String) = part
    .replace(' ', '+')
    .replace("*", "%2A")
    .replace("%7E", "~")
    .replace("%7e", "~")
    .replace("|", "%7C")

  private val keyValueAmpRemainderRegEx = """^([^=]*)=([^&]*)&amp;(.*)$""".r
  private val keyValueRemainderRegEx    = """^([^=]*)=([^&]*)&(.*)$""".r
  private val keyValueRegEx             = """^([^=]*)=(.*)$""".r

  private def parseQuery(query: String, keyValueList: List[(String, String)]): List[(String, String)] = query match {
    case keyValueAmpRemainderRegEx(key, value, remainder) => parseQuery(remainder, (key, value) :: keyValueList)
    case keyValueRemainderRegEx(key, value, remainder) => parseQuery(remainder, (key, value) :: keyValueList)
    case keyValueRegEx(key, value) => (key, value) :: keyValueList
    case _ if query.isEmpty => keyValueList
    case _ => throw new IllegalArgumentException("Failed to parse a URI query string: " + query)
  }

  /**
   * Resolves a child URI against a base URI.
   *
   * @param base the base URI
   * @param child the child URI
   * @return the resolved URI
   */
  def resolve(base: URI, child: URI): URI = {
    if (!base.hasPath || !child.hasPath || child.hasScheme) {
      child
    } else if ( // child
      !child.hasScheme
        && child.hasHost
        && child.hasPath
        && child.getPath().isEmpty
        && child.hasFragment
        && !child.hasQuery
    ) {
      if (base.hasFragment && base.getFragment().equals(child.getFragment())) {
        base
      } else {
        new URI(base.scheme, base.userInfo, base.host, base.port, base.path, base.query, child.fragment)
      }
    } else {
      new URI(
        base.scheme,
        if (child.hasHost) child.userInfo else base.userInfo,
        if (child.hasHost) child.host else base.host,
        if (child.hasHost) child.port else base.port,
        normalizePath(resolvePath(base, child)),
        child.query,
        child.fragment
      )
    }
  }

  private def resolvePath(base: URI, child: URI): Option[String] = {
    if (child.hasHost) {
      child.path
    } else {
      if (child.isPathAbsolute) {
        child.path
      } else {
        resolveRelativePath(base, child)
      }
    }
  }

  private def resolveRelativePath(base: URI, child: URI): Option[String] = if (base.hasPath) {
    val basePathStr = base.getPath()
    val lastSlashIndex = basePathStr.lastIndexOf('/')
    Some((if (lastSlashIndex >= 0) basePathStr.substring(0, lastSlashIndex + 1) else "/") + child.getPath())
  } else {
    child.path
  }
}

/**
 * An object representing a Uniform Resource Identifier (URI)
 *
 * @param scheme an optional URI scheme (e.g. http, ftp, s3)
 * @param userInfo an optional user-info (e.g. foobar)
 * @param host an optional host (e.g. www.google.com)
 * @param port an optional port (e.g. 8080)
 * @param path an optional path (e.g. /foo/bar/baz)
 * @param query an optional query string (e.g. foo=1&bar="baz")
 * @param fragment an optional fragment (e.g. foobar)
 */
class URI(
           val scheme: Option[String],
           val userInfo: Option[String],
           val host: Option[String],
           val port: Option[Int],
           val path: Option[String],
           val query: Option[String],
           val fragment: Option[String]
           ) {
  lazy val string: String = {
    val sb = new StringBuffer

    if (scheme.isDefined) sb.append(getScheme()).append(':')
    if (hasHost) {
      sb.append("//")
      if (hasUserInfo) sb.append(getUserInfo()).append('@')
      val needBrackets =
        getHost().indexOf(':') >= 0 &&
          !getHost().startsWith("[") &&
          !getHost().endsWith("]")
      if (needBrackets) sb.append('[')
      sb.append(getHost())
      if (needBrackets) sb.append(']')
      if (hasPort) sb.append(':').append(getPort())
    }
    if (path.isDefined) sb.append(getPath())
    if (query.isDefined) sb.append('?').append(getQuery())
    if (fragment.isDefined) sb.append('#').append(getFragment())

    sb.toString
  }

  val domainExtractor = """^.*\.([^.]*\.[^.]*$)""".r

  /**
   * Returns this URI's domain, which is the last to parts of the URI's host (if defined). For example,
   * the domain for the host www.google.com is google.com.
   *
   * @return
   */
  def domain: Option[String] = if (hasHost) getHost() match {
    case domainExtractor(domain) => Some(domain)
    case _ => host
  } else {
    None
  }

  /**
   * Returns this URI's file extension (if defined). The file extension is taken from the URI's path.
   *
   * @return this URI's file extension
   */
  def fileExtension: Option[String] = {
    val path = getPath()
    val lastDotIndex = path.lastIndexOf(".")
    if (lastDotIndex > 0 && // has a dot
      lastDotIndex >= path.length - 5 && // near the end of the string
      lastDotIndex < (path.length - 1)) { // is not the last char in the string
      Some(path.substring(lastDotIndex + 1))
    } else {
      None
    }
  }

  def isAbsolute = hasScheme
  def isOpaque = !hasPath

  def hasScheme = scheme.isDefined
  def hasUserInfo = userInfo.isDefined
  def hasHost = host.isDefined
  def hasPort = port.isDefined
  def hasPath = path.isDefined && !getPath().isEmpty
  def hasQuery = query.isDefined && !getQuery().isEmpty
  def hasFragment = fragment.isDefined && !getFragment().isEmpty

  private def getPathComponentsAndIndex(index: Int) = {
    val pathComponents = getPath().split('/').toList
    val actualIndex  = if (index < 0) pathComponents.size + index else index
    (pathComponents, actualIndex)
  }

  def getScheme(default: String = "")        = scheme.getOrElse(default)
  def getUserInfo(default: String = "")      = userInfo.getOrElse(default)
  def getHost(default: String = "")          = host.getOrElse(default)
  def getDomain(default: String = "")        = domain.getOrElse(default)
  def getPort(default: Int = -1)             = port.getOrElse(default)
  def getPath(default: String = "")          = path.getOrElse(default)
  def getPathComponent(index: Int): Option[String] = {
    val (pathComponents, actualIndex) = getPathComponentsAndIndex(index)
    if (actualIndex < pathComponents.size) {
      Some(pathComponents(actualIndex))
    } else {
      None
    }
  }
  def getFileExtension(default: String = "") = fileExtension.getOrElse(default)
  def getQuery(default: String = "") = query.getOrElse(default)
  def getQueryMap = getQuery().split('&').map(keyEqualValue => keyEqualValue.split('=')).map(keyValue =>
    keyValue(0) -> (if (keyValue.size > 1) keyValue(1) else "")
  ).toMap

  def getQueryValue(key: String) = getQueryMap.get(key)
  def getFragment(default: String = "") = fragment.getOrElse(default)

  def isPathAbsolute = hasPath && getPath().charAt(0) == '/'

  def resolve(child: URI): URI = URI.resolve(this, child)

  def withScheme(scheme: String) = URI(Option(scheme), userInfo, host, port, path, query, fragment)
  def withHost(host: String) = URI(scheme, userInfo, Option(host), port, path, query, fragment)
  def withPath(path: String) = URI(scheme, userInfo, host, port, Option(path), query, fragment)
  def withPathComponent(index: Int, pathComponent: String) = {
    val (pathComponents, actualIndex) = getPathComponentsAndIndex(index)
    val newPath = (pathComponents.take(actualIndex) ::: pathComponent :: pathComponents.drop(actualIndex + 1)).mkString("/")
    withPath(newPath)
  }
  def withQuery(query: String): URI = URI(scheme, userInfo, host, port, path, Option(query), fragment)
  def withQuery(map: Map[String, Any]): URI = withQuery(map.map {
    case (key, value) => key + "=" + value.toString
  }.mkString("&"))
  def withQueryValue(key: String, value: String) = withQuery(getQueryMap.updated(key, value))

  def withJustPath    = URI(None, None, None, None, path, None, None)
  def withJustPathAndQuery = URI(None, None, None, None, path, query, None)
  def withoutQuery    = if (hasQuery)    URI(scheme, userInfo, host, port, path, None, fragment) else this
  def withoutFragment = if (hasFragment) URI(scheme, userInfo, host, port, path, query, None) else this

  def toURL: URL = if (isAbsolute) new URL(toString) else throw new IllegalArgumentException("URI is not absolute.")
  def toURL(cls: Class[_]): URL = if (isAbsolute) toURL else cls.getResource(getPath())

  def toSource: Source = Source.fromURL(toURL)
  def toSource(cls: Class[_]) = Source.fromURL(toURL(cls))

  override def toString = string

  override def hashCode = toString.hashCode

  override def equals(other: Any) = other match {
    case that: URI => toString == that.toString
    case _ => false
  }
}

private object Authority {
  private val userInfoHostPortRegEx = """^([^@]*)@([^:]*):([0-9]*)$""".r
  private val userInfoHostRegEx = """^([^@]*)@(.*)$""".r
  private val hostPortRegEx = """^([^:]*):([0-9]*)$""".r
  private val hostRegEx = """^(.*)$""".r

  def apply(userInfo: Option[String], host: Option[String], port: Option[Int]): Authority = new Authority(userInfo, host, port)

  def apply(authority: String): Authority = authority match {
    case userInfoHostPortRegEx(userInfo, host, port) => apply(
      Some(userInfo),
      Some(host),
      Some(port.toInt)
    )
    case userInfoHostRegEx(userInfo, host) => apply(
      Some(userInfo),
      Some(host),
      None
    )
    case hostPortRegEx(host, port) => apply(
      None,
      Some(host),
      Some(port.toInt)
    )
    case hostRegEx(host) => apply(
      None,
      Some(host),
      None
    )
    case _ => apply(None, None, None)
  }
}

private class Authority(val userInfo: Option[String], val host: Option[String], val port: Option[Int])

