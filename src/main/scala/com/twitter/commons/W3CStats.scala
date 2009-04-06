/** Copyright 2009, Twitter, Inc */
package com.twitter.commons

import scala.collection.mutable
import java.util.Date
import java.net.InetAddress
import java.text.SimpleDateFormat

/**
 * Implements a W3C Stats Log.
 *
 * @param fields The fields, in order, as they will appear in the final w3c log output.
 */
class W3CStats(val fields: Array[String]) {
  //val fields = Array("backend-response-time", "backend-response-method", "request-uri")

  /**
   * Store our map of named events.
   */
  private val tl = new ThreadLocal[mutable.Map[String, Any]]() {
    override def initialValue(): mutable.Map[String, Any] = new mutable.HashMap[String, Any]
  }

  def get: mutable.Map[String, Any] = tl.get()

  /**
   * Adds the current name, value pair to the stats map.
   */
  def log(name: String, value: String) {
    get + (name -> value)
  }

  /**
   * Adds the current name, timing pair to the stats map.
   */
  def log(name: String, timing: Long) {
    get + (name -> timing)
  }

  def log(name: String, date: Date) {
    log(name, date_format(date))
  }

  def log(name: String, ip: InetAddress) {
    get + (name -> ip)
  }

  def valueOrSupplied(name: String, ifNil: String) = {
    get.get(name) match {
      case Some(s) => s
      case None => ifNil
    }
  }


  /**
   * Returns a w3c logline.
   */
  def log_entry: String = {
    val logline = new StringBuilder()
    // Yes, that's _3_ gets. The first is on ThreadLocal, the second is on Map, the third is on Option.
    // FIXME: don't use Option.get. Instead check that it exists and use a - if it doesn't.
    fields.foreach(field => { logline.append(valueOrSupplied(field, "-")) ; logline.append(" ") } )
    logline.toString
  }

  /**
   * Returns an String containing W3C Headers separated by newlines.
   */
  def log_header: String = {
    Array("#Version: 1.0",
          date_header(),
          "#CRC: abc123",
          fields_header()).mkString("\n")
  }

  /**
   * Returns the Date Header with the current time.
   */
  def date_header(): String = {
    date_header(new Date())
  }

  /**
   * Returns the W3C Extended Log Format Date Header.
   */
  def date_header(date: Date): String = {
    "#Date: %s".format(date_format(date))
  }

  /**
   * Formats the supplied date
   */
  def date_format(date: Date): String = {
    val formatter = new SimpleDateFormat("dd-MMM-yyyy HH:mm:ss")
    formatter.format(date)
  }


  /**
   * Returns a W3C Fields header.
   */
  def fields_header(): String = {
    val header = new StringBuffer("#Fields: ")
    header.append(fields.mkString(" "))
    header.toString
  }

  /**
   * Runs the function f, times how long it took, and logs that duration, in milliseconds, with the given name.
   */
  def time[T](name: String)(f: => T): T = {
    val (rv, duration) = Stats.time(f)
    log(name, duration)
    rv
  }

  /**
   * Runs the function f, times how long it took, and logs that duration, in nanoseconds, with the given name.
   *
   * When using nanoseconds, be sure to encode your field with that fact. Consider
   * using the suffix _ns in your field.
   */
  def timeNanos[T](name: String)(f: => T): T = {
    val (rv, duration) = Stats.timeNanos(f)
    log(name, duration)
    rv
  }
}
