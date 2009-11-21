/*
 * Copyright 2009 Twitter, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.twitter.stats

import java.lang.management._
import java.util.concurrent.atomic.AtomicLong
import scala.collection.{Map, mutable, immutable}
import com.twitter.json.Json
import com.twitter.xrayspecs.Time
import net.lag.logging.Logger


/**
 * Basic Stats gathering object that returns performance data for the application.
 */
object Stats extends StatsProvider {
  val log = Logger.get(getClass.getName)

  /**
   * A gauge has an instantaneous value (like memory usage) and is polled whenever stats
   * are collected.
   */
  trait Gauge extends ((Boolean) => Double)

  // Maintains a Map of the variables we are tracking and their value.
  private val counterMap = new mutable.HashMap[String, Counter]()
  private val timingMap = new mutable.HashMap[String, Timing]()
  private val timingStatsMap = new mutable.HashMap[String, TimingStat]()
  private val gaugeMap = new mutable.HashMap[String, Gauge]()

  // Maintains a list of callback functions for fetching TimingStat objects.
  private var timingStatsFnList = new mutable.ListBuffer[(Boolean) => Map[String, TimingStat]]()

  def clearAll() = {
    counterMap.synchronized { counterMap.clear() }
    timingMap.synchronized { timingMap.clear() }
    timingStatsMap.synchronized { timingStatsMap.clear() }
    gaugeMap.synchronized { gaugeMap.clear() }
    clearTimingStatsFn()
  }

  /**
   * Removes all timingStats callback functions.
   */
  def clearTimingStatsFn() {
    timingStatsFnList.synchronized { timingStatsFnList.clear() }
  }

  /**
   * Find or create a counter with the given name.
   */
  def getCounter(name: String): Counter = counterMap.synchronized {
    counterMap.get(name) match {
      case Some(counter) => counter
      case None =>
        val counter = new Counter
        counterMap += (name -> counter)
        counter
    }
  }

  /**
   * Find or create a timing measurement with the given name.
   */
  def getTiming(name: String): Timing = timingMap.synchronized {
    timingMap.get(name) match {
      case Some(timing) => timing
      case None =>
        val timing = new Timing
        timingMap += (name -> timing)
        timing
    }
  }

  /**
   * Create a gauge with the given name.
   */
  def makeGauge(name: String)(gauge: => Double): Unit = gaugeMap.synchronized {
    gaugeMap += (name -> new Gauge { def apply(reset: Boolean) = gauge })
  }

  def makeDerivativeGauge(name: String, nomCounter: Counter, denomCounter: Counter): Unit = {
    val g = new Gauge {
      var lastNom: Long = 0
      var lastDenom: Long = 0

      def apply(reset: Boolean) = {
        val nom = nomCounter.value.get
        val denom = denomCounter.value.get
        val deltaNom = nom - lastNom
        val deltaDenom = denom - lastDenom
        if (reset) {
          lastNom = nom
          lastDenom = denom
        }
        if (deltaDenom == 0) 0.0 else deltaNom * 1.0 / deltaDenom
      }
    }
    timingMap.synchronized {
      gaugeMap += (name -> g)
    }
  }

  /**
   * Returns a function that increments the named counter by 1.
   */
  def buildIncr(name: String): () => Long = { () => incr(name, 1) }

  /**
   * Increments the named counter by `by`.
   */
  def incr(name: String, by: Int): Long = {
    getCounter(name).value.addAndGet(by)
  }

  /**
   * Register a function to be called when you want to collect timing stats.
   * The Boolean parameter is whether the state should be 'reset'
   */
  def registerTimingStatsFn(fn: (Boolean) => Map[String, TimingStat]) = timingStatsFnList.synchronized {
    timingStatsFnList += fn
  }

  def addTiming(name: String, duration: Int): Long = {
    getTiming(name).add(duration)
  }

  /**
   * Adds an immutable TimingStat instance with a given name.
   */
  def addTimingStat(name: String, stat: TimingStat) = timingStatsMap.synchronized {
    timingStatsMap += (name -> stat)
  }

  /**
   * Returns how long it took, in milliseconds, to run the function f.
   */
  def duration[T](f: => T): (T, Long) = {
    val start = Time.now
    val rv = f
    val duration = Time.now - start
    (rv, duration.inMilliseconds)
  }

  /**
   * Returns how long it took, in nanoseconds, to run the function f.
   */
  def durationNanos[T](f: => T): (T, Long) = {
    val start = System.nanoTime
    val rv = f
    val duration = System.nanoTime - start
    (rv, duration)
  }

  /**
   * Returns a Map[String, Long] of JVM stats.
   */
  def getJvmStats(): Map[String, Long] = {
    val out = new mutable.HashMap[String, Long]
    val mem = ManagementFactory.getMemoryMXBean()

    val heap = mem.getHeapMemoryUsage()
    out += ("heap_committed" -> heap.getCommitted())
    out += ("heap_max" -> heap.getMax())
    out += ("heap_used" -> heap.getUsed())

    val nonheap = mem.getNonHeapMemoryUsage()
    out += ("nonheap_committed" -> nonheap.getCommitted())
    out += ("nonheap_max" -> nonheap.getMax())
    out += ("nonheap_used" -> nonheap.getUsed())

    val threads = ManagementFactory.getThreadMXBean()
    out += ("thread_daemon_count" -> threads.getDaemonThreadCount().toLong)
    out += ("thread_count" -> threads.getThreadCount().toLong)
    out += ("thread_peak_count" -> threads.getPeakThreadCount().toLong)

    val runtime = ManagementFactory.getRuntimeMXBean()
    out += ("start_time" -> runtime.getStartTime())
    out += ("uptime" -> runtime.getUptime())

    val os = ManagementFactory.getOperatingSystemMXBean()
    out += ("num_cpus" -> os.getAvailableProcessors().toLong)

    out
  }

  def getCounterStats(reset: Boolean): Map[String, Long] = {
    val rv = immutable.HashMap(counterMap.map { case (k, v) => (k, v.value.get) }.toList: _*)
    if (reset) {
      for ((k, v) <- counterMap) {
        v.reset()
      }
    }
    rv
  }

  def getTimingStats(reset: Boolean): Map[String, TimingStat] = {
    val out = new mutable.HashMap[String, TimingStat]

    for ((key, timing) <- timingMap) {
      out += (key -> timing.get(reset))
    }

    val stats = timingStatsFnList.flatMap(_(reset).toList)
    for ((key, timing) <- stats ++ timingStatsMap) {
      out += (key + new TimingStat(timing.count, timing.minimum, timing.maximum, timing.sum, timing.sumSquares))
    }

    out
  }

  /**
   * Returns a Map[String, Double] of current gauge readings.
   */
  def getGaugeStats(reset: Boolean): Map[String, Double] = {
    immutable.HashMap(gaugeMap.map(x => (x._1, x._2(reset))).toList: _*)
  }

  /**
   * Returns a formatted String containing Memory statistics of the form
   * name: value
   */
  def stats(reset: Boolean): String = {
    val out = new mutable.ListBuffer[String]()
    for ((key, value) <- getJvmStats()) {
      out += (key + ": " + value.toString)
    }
    for ((key, value) <- getCounterStats()) {
      out += (key + ": " + value.toString)
    }
    for ((key, value) <- getTimingStats(reset)) {
      out += (key + ": " + value.toString)
    }
    for ((key, value) <- getGaugeStats(reset)) {
      out += (key + ": " + value.toString)
    }
    out.mkString("\n")
  }

  def stats(): String = stats(true)

  def jsonStats(reset: Boolean): String = Json.build(stats(reset)).toString
  def jsonStats(): String = jsonStats(true)
}
