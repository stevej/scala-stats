/** Copyright 2009 Twitter, Inc. */
package com.twitter.service

import net.lag.extensions._
import org.specs._
import scala.collection.immutable
import Stats.TimingStat

object StatsSpec extends Specification {
  "Stats" should {
    doBefore {
      Stats.clearAll()
    }

    "jvm stats" in {
      val jvmStats = Stats.getJvmStats()
      jvmStats.keys.toList must contain("num_cpus")
      jvmStats.keys.toList must contain("heap_used")
      jvmStats.keys.toList must contain("start_time")
    }

    "counters" in {
      Stats.incr("widgets", 1)
      Stats.incr("wodgets", 12)
      Stats.incr("wodgets")
      Stats.getCounterStats() mustEqual Map("widgets" -> 1, "wodgets" -> 13)
    }

    "timings" in {
      "empty" in {
        Stats.addTiming("test", 0)
        val test = Stats.getTiming("test")
        test.getCountMinMaxAvg(true) mustEqual((1L, 0, 0, 0))
        // the timings list will be empty here:
        test.getCountMinMaxAvg(true) mustEqual((0L, 0, 0, 0))
      }

      "basic min/max/average" in {
        Stats.addTiming("test", 1)
        Stats.addTiming("test", 2)
        Stats.addTiming("test", 3)
        val test = Stats.getTiming("test")
        test.getCountMinMaxAvg(true) mustEqual ((3L, 1, 3, 2))
      }

      "report" in {
        var x = 0
        Stats.time("hundred") { for (i <- 0 until 100) x += i }
        val timings = Stats.getTimingStats(false)
        timings.keys.toList must containAll(List("hundred_min", "hundred_max", "hundred_count", "hundred_avg"))
        timings("hundred_count") mustEqual 1
        timings("hundred_min") mustEqual timings("hundred_avg")
        timings("hundred_max") mustEqual timings("hundred_avg")
      }

      "average of 0" in {
        Stats.addTiming("test", 0)
        val test = Stats.getTiming("test")
        test.getCountMinMaxAvg(true) mustEqual((1L, 0, 0, 0))
      }

      "ignore negative timings" in {
        Stats.addTiming("test", 1)
        Stats.addTiming("test", -1)
        Stats.addTiming("test", Math.MIN_INT)
        val test = Stats.getTiming("test")
        test.getCountMinMaxAvg(true) must beEqual((1L, 1, 1, 1))
      }

      "boundary timing sizes" in {
        Stats.addTiming("test", Math.MAX_INT)
        Stats.addTiming("test", 5)
        val test = Stats.getTiming("test")
        test.getCountMinMaxAvg(true) must beEqual((2L, 5, Math.MAX_INT, ((5L + Math.MAX_INT) / 2L).toInt))
      }

      "handle code blocks" in {
        Stats.time("test") {
          Thread.sleep(10)
        }
        val test = Stats.getTiming("test")
        test.getCountMinMaxAvg(true)._4 must be_>=(10)
      }

      "reset when asked" in {
        var x = 0
        Stats.time("hundred") { for (i <- 0 until 100) x += i }
        Stats.getTimingStats(false)("hundred_count") mustEqual 1
        Stats.time("hundred") { for (i <- 0 until 100) x += i }
        Stats.getTimingStats(false)("hundred_count") mustEqual 2
        Stats.getTimingStats(true)("hundred_count") mustEqual 2
        Stats.time("hundred") { for (i <- 0 until 100) x += i }
        Stats.getTimingStats(false)("hundred_count") mustEqual 1
      }

      "timing stats can be added and reflected in Stats.getTimingStats" in {
        var x = 0
        Stats.time("hundred") { for (i <- 0 until 100) x += 1 }
        Stats.getTimingStats(false).size mustEqual 4
        Stats.addTimingStat("foobar", TimingStat(1, 0, 0, 0))
        Stats.getTimingStats(false).size mustEqual 8
        Stats.getTimingStats(true)("foobar_count") mustEqual 1
        Stats.addTimingStat("foobar", TimingStat(3, 0, 0, 0))
        Stats.getTimingStats(false)("foobar_count") mustEqual 3
      }

      "timing stats callbacks are called" in {
        var i = 0
        var j = 0
        var resetCalled = 0

        Stats.registerTimingStatsFn { reset =>
          i += 1
          j += 1
          resetCalled += 1
          Map(("hello" -> TimingStat(0, 1, 2, 3)))
        }

        Stats.registerTimingStatsFn { reset =>
          j += 1
          resetCalled += 1
          Map(("goodbye" -> TimingStat(4, 5, 6, 7)))
        }

        val stats = Stats.getTimingStats(false)
        stats.size mustEqual 8
        stats("hello_count") mustEqual 0
        stats("hello_min") mustEqual 1
        stats("hello_max") mustEqual 2
        stats("hello_avg") mustEqual 3
        stats("goodbye_count") mustEqual 4
        stats("goodbye_min") mustEqual 5
        stats("goodbye_max") mustEqual 6
        stats("goodbye_avg") mustEqual 7
        i mustEqual 1
        j mustEqual 2
        resetCalled mustEqual 2
      }
    }

    "gauges" in {
      "report" in {
        Stats.makeGauge("pi") { java.lang.Math.PI }
        Stats.getGaugeStats(false) mustEqual Map("pi" -> java.lang.Math.PI)
      }

      "update" in {
        var potatoes = 100.0
        // gauge that increments every time it's read:
        Stats.makeGauge("stew") { potatoes += 1.0; potatoes }
        Stats.getGaugeStats(true) mustEqual Map("stew" -> 101.0)
        Stats.getGaugeStats(true) mustEqual Map("stew" -> 102.0)
        Stats.getGaugeStats(true) mustEqual Map("stew" -> 103.0)
      }

      "derivative" in {
        Stats.incr("results", 100)
        Stats.incr("queries", 25)
        Stats.makeDerivativeGauge("results_per_query", Stats.getCounter("results"),
                                  Stats.getCounter("queries"))
        Stats.getGaugeStats(true) mustEqual Map("results_per_query" -> 4.0)
        Stats.getGaugeStats(true) mustEqual Map("results_per_query" -> 0.0)
        Stats.incr("results", 10)
        Stats.incr("queries", 5)
        Stats.getGaugeStats(false) mustEqual Map("results_per_query" -> 2.0)
        Stats.getGaugeStats(false) mustEqual Map("results_per_query" -> 2.0)
      }
    }

    "report to JMX" in {
      Stats.incr("widgets", 1)
      Stats.time("nothing") { 2 * 2 }

      val mbean = new StatsMBean
      val names = mbean.getMBeanInfo().getAttributes().toList.map { _.getName() }
      names must containAll(List("counter_widgets", "timing_nothing_min", "timing_nothing_max",
                                 "timing_nothing_avg", "timing_nothing_count"))
    }
  }
}
