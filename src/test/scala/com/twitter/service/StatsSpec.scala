/** Copyright 2009 Twitter, Inc. */
package com.twitter.service

import net.lag.extensions._
import org.specs._
import scala.collection.immutable


object StatsSpec extends Specification {
  "Stats" should {
    doBefore {
      Stats.clearAll()
    }

    "report jvm stats" in {
      val jvmStats = Stats.getJvmStats()
      jvmStats.keys.toList must contain("num_cpus")
      jvmStats.keys.toList must contain("heap_used")
      jvmStats.keys.toList must contain("start_time")
    }

    "report counters" in {
      Stats.incr("widgets", 1)
      Stats.incr("wodgets", 12)
      Stats.incr("wodgets")
      Stats.getCounterStats() mustEqual Map("widgets" -> 1, "wodgets" -> 13)
    }

    "report timings" in {
      var x = 0
      Stats.time("hundred") { for (i <- 0 until 100) x += i }
      val timings = Stats.getTimingStats(false)
      timings.keys.toList mustEqual List("hundred")
      timings("hundred").count mustEqual 1
      timings("hundred").minimum mustEqual timings("hundred").average
      timings("hundred").maximum mustEqual timings("hundred").average
    }

    "reset timings when asked" in {
      var x = 0
      Stats.time("hundred") { for (i <- 0 until 100) x += i }
      Stats.getTimingStats(false)("hundred").count mustEqual 1
      Stats.time("hundred") { for (i <- 0 until 100) x += i }
      Stats.getTimingStats(false)("hundred").count mustEqual 2
      Stats.getTimingStats(true)("hundred").count mustEqual 2
      Stats.time("hundred") { for (i <- 0 until 100) x += i }
      Stats.getTimingStats(false)("hundred").count mustEqual 1
    }

    "report gauges" in {
      Stats.makeGauge("pi") { java.lang.Math.PI }
      Stats.getGaugeStats(false) mustEqual Map("pi" -> java.lang.Math.PI)
    }

    "report to JMX" in {
      Stats.incr("widgets", 1)
      Stats.time("nothing") { 2 * 2 }

      val mbean = new StatsMBean
      val names = mbean.getMBeanInfo().getAttributes().toList.map { _.getName() }
      names mustEqual List("counter_widgets", "timing_min_nothing", "timing_max_nothing", "timing_average_nothing", "timing_count_nothing")
    }
  }
}
