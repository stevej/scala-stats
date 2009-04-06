/** Copyright 2009 Twitter, Inc. */
package com.twitter.commons

import net.lag.extensions._
import org.specs._
import scala.collection.immutable
import java.util.Date


object W3CStatsSpec extends Specification {
  "w3c Stats" should {
    val w3c = new W3CStats(Array("backend-response-time", "backend-response-method", "request-uri"))
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

      val logline = w3c.log_entry
      logline mustNot beNull

      val entries: Array[String] = logline.split(" ")
      entries(0).toInt must be_>=(0)
      entries(1) mustEqual "GET"
      entries(2) mustEqual "/home"
    }

    "date_header uses the w3c format" in {
      val header = w3c.date_header(new Date(0))
      header mustEqual "#Date: 31-Dec-1969 16:00:00"
    }
  }
}
