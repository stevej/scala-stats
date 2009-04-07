/** Copyright 2009 Twitter, Inc. */
package com.twitter.commons

import net.lag.extensions._
import org.specs._
import scala.collection.immutable
import java.util.Date


object W3CStatsSpec extends Specification {
  "w3c Stats" should {
    val w3c = new W3CStats(Array("backend-response-time", "backend-response-method", "request-uri", "backend-response-time_ns", "unsupplied-field", "finish_timestamp"))
    "add 2 numbers" in {
      1 + 1 mustEqual 2
    }

    "log some specific timings" in {
      val response: Int = w3c.time[Int]("backend-response-time") {
        w3c.log("backend-response-method", "GET")
        w3c.log("request-uri", "/home")
        1 + 1
      }
      response mustEqual 2
      w3c.log("finish_timestamp", new Date())
      val response2: Int = w3c.timeNanos[Int]("backend-response-time-nanoseconds") {
        1 + 2
      }
      response2 mustEqual 3

      val logline = w3c.log_entry
      //println(logline)
      logline mustNot beNull

      val entries: Array[String] = logline.split(" ")
      entries(0).toInt must be_>=(0)
      entries(1) mustEqual "GET"
      entries(2) mustEqual "/home"
      entries(3).toInt must be_>=(10)  //must take at least 10 ns!
      entries(4) mustEqual "-"
      entries(5) mustNot beNull
    }

    "date_header uses the w3c format" in {
      val header = w3c.date_header(new Date(0))
      header mustEqual "#Date: 31-Dec-1969 16:00:00"
    }

    "log_header is present and has Version and CRC" in {
      val log_header = w3c.log_header
      log_header mustNot beNull
    }
  }
}
