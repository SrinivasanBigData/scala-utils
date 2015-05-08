package com.sgw.scala.utils

import java.net.URL

import scala.io.Source

object CSVFile {
  /**
   * Creates a CSV file from a file identified by the specified URL. For files on the local filesystem, use a file URL
   * as in "file://foo/bar/baz.csv".
   *
   * @param path path to the CSV file.
   * @param sep the separator character.
   * @param key the column name used as the key field when looking up records in the file
   */
  def apply(path: String, sep: Char, key: String): CSVFile = {
    val uri = URI(path)
    val url = if (uri.isAbsolute) uri.toURL else getClass.getResource(path)
    apply(url, sep, key)
  }

  /**
   * Creates a CSV file from a file identified by the specified URL. For files on the local filesystem, use a file URL
   * as in "file://foo/bar/baz.csv".
   *
   * @param url the URL of the CSV file.
   * @param sep the separator character.
   * @param key the column name used as the key field when looking up records in the file
   */
  def apply(url: URL, sep: Char, key: String): CSVFile = {
    val lines = Source.fromURL(url).getLines().toList
    apply(lines, sep, key)
  }
}

/**
 * A class that maps a CSV file to a collection of records where each record contain fields values.
 * This class assumes that the CSV file has a header line (column names). The column names can be used to
 * identify values within a row. Also, one of the columns can be specified (through the key parameter) as
 * the column used to find rows by specifying a value within that column (called a "rowKey" in this
 * class's API).
 *
 * @param lines the CSV file's records including the header line.
 * @param sep the field separator
 * @param key the column name used as the key field when looking up records in this file
 */
case class CSVFile(lines: List[String], sep: Char, key: String) {
  /**
   * The array of column names found in the first row of the CSV file.
   */
  val columnNames = lines.head.split(sep + "(?=([^\"]*\"[^\"]*\")*[^\"]*$)")

  private val columnNameToIndexMap = columnNames.map(columnName => columnName.trim).zipWithIndex.toMap

  /**
   * The list of CSVRecords.
   */
  lazy val records = lines
    .drop(1) // drop the column-names line
    .map(line => CSVRecord(
    line
      .split(sep + "(?=([^\"]*\"[^\"]*\")*[^\"]*$)")
      .map(value => if (value.startsWith("\"") && value.endsWith("\"")) { // remove surrounding quotes
      value.substring(1, value.length-1)
    } else {
      value
    })
  )
    )

  private val keyIndex = columnNameToIndexMap(key)
  private val keyToRecordMap = records.map(record => (record(keyIndex), record)).toMap

  /**
   * The number of rows in the CSV file not including the column names row.
   */
  def size = records.size

  /**
   * The number of columns in the CSV file.
   */
  def width = columnNames.size

  /**
   * Returns a CSVRecord for the given row number (zero based)
   * @param row the row index
   * @throws IndexOutOfBoundsException if the row index is less-than zero or greater than size - 1
   */
  def apply(row: Int): CSVRecord = records(row)

  /**
   * Return the value at the specified row and column (zero based).
   *
   * @param row the row index
   * @param col the column index
   * @throws IndexOutOfBoundsException if the row or column index is less-than zero or greater than size - 1
   */
  def apply(row: Int, col: Int): String = records(row)(col)

  /**
   * Returns the value at the specified row index and column where the column is one of the names from the columnNames array.
   * @param row the row index
   * @param columnName a column name
   * @throws IndexOutOfBoundsException if the row index is less-than zero or greater than size - 1
   */
  def apply(row: Int, columnName: String): String = records(row)(columnName)

  /**
   * Returns a CSVRecord for a row identified by the specified rowKey found in the column with the name matching this
   * CSVFile's "key" attribute.
   *
   * @param rowKey the row key
   * @throws NoSuchElementException if the rowKey can not be found
   */
  def apply(rowKey: String): CSVRecord = keyToRecordMap(rowKey)

  /**
   * Returns the value at the specified row and column.
   * @param rowKey the row key
   * @param col a column index
   *
   * @throws NoSuchElementException if the rowKey can not be found
   * @throws IndexOutOfBoundsException if the column index is less-than zero or greater than size - 1
   */
  def apply(rowKey: String, col: Int) = keyToRecordMap(rowKey)(col)

  /**
   * Returns the value at the specified row and column.
   * @param rowKey the row key
   * @param columnName a column name
   *
   * @throws NoSuchElementException if the rowKey or columnName can not be found
   */
  def apply(rowKey: String, columnName: String): String = keyToRecordMap(rowKey)(columnName)

  /**
   * Returns a sequence of values at the specified row matching the specified column names.
   * @param rowKey the row key
   * @param columnNames a list of column names
   *
   * @throws NoSuchElementException if the row key or any of the column names can not be found
   */
  def apply(rowKey: String, columnNames: String*): Seq[String] = {
    val row = keyToRecordMap(rowKey)
    columnNames.map(columnName => row(columnName))
  }

  /**
   * Returns a pair of values at the specified row matching the specified pair of column names.
   * @param rowKey the row key
   * @param columnNames a list of column names
   *
   * @throws NoSuchElementException if the row key or any of the column names can not be found
   */
  def apply(rowKey: String, columnNames: (String, String)): (String, String) = {
    val row = keyToRecordMap(rowKey)
    (row(columnNames._1), row(columnNames._2))
  }

  /**
   * Returns a three-tuple of values at the specified row matching the specified three-tuple of column names.
   * @param rowKey the row key
   * @param columnNames a list of column names
   *
   * @throws NoSuchElementException if the row key or any of the column names can not be found
   */
  def apply(rowKey: String, columnNames: (String, String, String)): (String, String, String) = {
    val row = keyToRecordMap(rowKey)
    (row(columnNames._1), row(columnNames._2), row(columnNames._3))
  }

  /**
   * Returns an optional CSVRecord for the specified row key. If the row key can't be found, then this method
   * return None, otherwise it returns a Some(CSVRecord).
   * @param rowKey the row key
   */
  def get(rowKey: String): Option[CSVRecord] = keyToRecordMap.get(rowKey)

  /**
   * Returns an optional CSVRecord for the specified row key and column name. If the row key or column name can't be found, then this method
   * return None, otherwise it returns a Some(String).
   * @param rowKey the row key
   * @param columnName the column name
   */
  def get(rowKey: String, columnName: String): Option[String] = get(rowKey).flatMap(_.get(columnName))

  /**
   * Returns true if the row specified by the row key exists; false otherwise.
   * @param rowKey the row key
   */
  def contains(rowKey: String): Boolean = keyToRecordMap.contains(rowKey)

  /**
   * Returns true if the value specified by the row key and column name exists; false otherwise.
   * @param rowKey the row key
   */
  def contains(rowKey: String, columnName: String): Boolean = contains(rowKey) && columnNameToIndexMap.contains(columnName)

  /**
   * An object that provides access to a single record in a CSVFile.
   *
   * @param record the record's array of Strings
   */
  case class CSVRecord(record: Array[String]) {
    def apply(col: Int): String = record(col)
    def apply(colKey: String): String = apply(columnNameToIndexMap(colKey))

    def get(colKey: String): Option[String] = columnNameToIndexMap.get(colKey).map(col => apply(col))
  }
}
