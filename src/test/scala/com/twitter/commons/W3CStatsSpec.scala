/** Copyright 2009 Twitter, Inc. */
package com.twitter.commons

import net.lag.extensions._
import org.specs._
import scala.collection.immutable
import java.util.Date


object W3CStatsSpec extends Specification {
  "w3c Stats" should {
    val w3c = new W3CStats(Array("backend-response-time", "backend-response-method", "request-uri", "backend-response-time_ns", "unsupplied-field", "finish_timestamp"))

    "log and check some timings" in {
      val response: Int = w3c.time[Int]("backend-response-time") {
        w3c.log("backend-response-method", "GET")
        w3c.log("request-uri", "/home")
        1 + 1
      }
      response mustEqual 2

      w3c.log("finish_timestamp", new Date(0))

      val response2: Int = w3c.timeNanos[Int]("backend-response-time_ns") {
        1 + 2
      }
      response2 mustEqual 3

      val logline = w3c.log_entry
      logline mustNot beNull

      val entries: Array[String] = logline.split(" ")
      entries(0).toInt must be_>=(0)
      entries(1) mustEqual "GET"
      entries(2) mustEqual "/home"
      entries(3).toInt must be_>=(10)  //must take at least 10 ns!
      entries(4) mustEqual "-"
      entries(5) mustEqual "31-Dec-1969_16:00:00"
    }

    "date_header uses the w3c format" in {
      val header = w3c.date_header(new Date(0))
      header mustEqual "#Date: 31-Dec-1969 16:00:00"
    }

    "crc_header is stable" in {
      w3c.crc32_header(w3c.fields_header()) mustEqual "#CRC: 841190001"
    }

    "fields_header is stable" in {
      val header = "#Fields: backend-response-time backend-response-method request-uri " +
      "backend-response-time_ns unsupplied-field finish_timestamp"
      w3c.fields_header() mustEqual header
    }

    "log_header is present and has Version, Fields, and CRC" in {
      val log_header = w3c.log_header
      log_header must include("#Version")
      log_header must include("#CRC")
      log_header must include("#Fields")
    }

    "map when cleared returns the empty string" in {
      w3c.log("request-uri", "foo")
      w3c.clear()
      val logline = w3c.log_entry
      // strip out all unfound entries, and remove all whitespace. after that, it should be empty.
      logline.replaceAll("-", "").trim() mustEqual ""
    }

    "datetime_format returns a tuple of Date and Time" in {
      val (date, time) = w3c.datetime_format(new Date(0))
      date mustEqual "31-Dec-1969"
      time mustEqual "16:00:00"
    }

    "logging a field not tracked in the fields member should throw an exception" in {
      w3c.log("jibberish_nonsense", "foo") must throwA(new IllegalArgumentException)
    }
  }
}
