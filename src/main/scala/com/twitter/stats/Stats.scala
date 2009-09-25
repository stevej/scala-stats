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
import java.util.logging.Logger
import scala.collection.{Map, mutable, immutable}
import com.twitter.json.{Json, JsonSerializable}


trait Stats {
  /**
   * Runs the function f and logs that duration, in milliseconds, with the given name.
   */
  def time[T](name: String)(f: => T): T

  /**
   * Runs the function f and logs that duration, in nanoseconds, with the given name.
   *
   * When using nanoseconds, be sure to encode your field with that fact. Consider
   * using the suffix `_ns` in your field.
   */
  def timeNanos[T](name: String)(f: => T): T

  /**
   * Increments a count in the stats.
   */
  def incr(name: String, count: Int): Long

  /**
   * Increments a count in the stats.
   */
  def incr(name: String): Long = incr(name, 1)
}


object DevNullStats extends Stats {
  def time[T](name: String)(f: => T): T = f
  def timeNanos[T](name: String)(f: => T): T = f
  def incr(name: String, count: Int): Long = count.toLong
}


/**
 * Basic Stats gathering object that returns performance data for the application.
 */
object Stats extends Stats {
  val log = Logger.getLogger("Stats")

  /**
   * Measurement is a base type for collected statistics.
   */
  trait Measurement

  /**
   * A Counter is a measure that simply keeps track of how
   * many times an event occurred.
   */
  class Counter extends Measurement {
    var value = new AtomicLong

    def incr() = value.addAndGet(1)
    def incr(n: Int) = value.addAndGet(n)
    def apply(): Long = value.get()
    def update(n: Long) = value.set(n)
    def reset() = update(0L)
  }


  /**
   * A Timing collates durations of an event and can report
   * min/max/avg along with how often the event occurred.
   */
  class Timing extends Measurement {
    private var maximum = Math.MIN_INT
    private var minimum = Math.MAX_INT
    private var sum: Long = 0
    private var count: Int = 0

    /**
     * Resets the state of this Timing. Clears the durations and counts collected sofar.
     */
    def clear() = synchronized {
      maximum = Math.MIN_INT
      minimum = Math.MAX_INT
      sum = 0
      count = 0
    }

    /**
     * Adds a duration to our current Timing.
     */
    def add(n: Int): Long = synchronized {
      if (n > -1) {
        maximum = n max maximum
        minimum = n min minimum
        sum += n
        count += 1
      } else {
        log.warning("Tried to add a negative timing duration. Was the clock adjusted?")
      }
      count
    }

    /**
     * Evaluates function f, returning value of type T, and logging the duration in the current Timer instance.
     */
    def time[T](f: => T): T = {
      val (rv, duration) = Stats.duration(f)
      add(duration.asInstanceOf[Int]) // is safe as long as it didn't take longer then 46 days.
      rv
    }

    /**
     * Returns a tuple of (Count, Min, Max, Average) for the measured event.
     * If `reset` is true, it clears the current event timings also.
     */
    def getCountMinMaxAvg(reset: Boolean): (Int, Int, Int, Int) = synchronized {
      if (count == 0) {
        (0, 0, 0, 0)
      } else {
        val average = (sum / count).toInt
        val rv = (count, minimum, maximum, average)
        if (reset) clear()
        rv
      }
    }
  }


  /**
   * A gauge has an instantaneous value (like memory usage) and is polled whenever stats
   * are collected.
   */
  trait Gauge extends ((Boolean) => Double) with Measurement


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
   * Increments the named counter by <code>by</code>.
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

  /**
   * Creates a Timing object of name and duration and stores
   * it in the keymap. Returns the total number of timings stored so far.
   */
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
   * Times the duration of function f, and adds that duration to a named timing measurement.
   */
  def time[T](name: String)(f: => T): T = {
    val (rv, msec) = duration(f)
    addTiming(name, msec.toInt)
    rv
  }

  /**
   * Times the duration of function f, and adds that duration to a named timing measurement.
   */
  def timeNanos[T](name: String)(f: => T): T = {
    val (rv, nsec) = durationNanos(f)
    addTiming(name, nsec.toInt)
    rv
  }

  /**
   * Returns how long it took, in milliseconds, to run the function f.
   */
  def duration[T](f: => T): (T, Long) = {
    val start = System.currentTimeMillis
    val rv = f
    val duration = System.currentTimeMillis - start
    (rv, duration)
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

  /**
   * Returns a Map[String, Long] of counters and their current values.
   */
  def getCounterStats(): Map[String, Long] = getCounterStats(false)

  /**
   * Returns a Map[String, Long] of counters and their current values.
   * @param reset whether or not to reset the counters.
   */
  def getCounterStats(reset: Boolean): Map[String, Long] = {
    val rv = immutable.HashMap(counterMap.map { case (k, v) => (k, v.value.get) }.toList: _*)
    if (reset) {
      for ((k, v) <- counterMap) {
        v.reset()
      }
    }
    rv
  }

  /**
   * A pre-calculated timing. E.g. if you have timing stats from an external source but
   * still want to report them via the Stats interface, then use a TimingStat.
   */
  case class TimingStat(count: Int, minimum: Int, maximum: Int, average: Int) extends JsonSerializable {
    def toJson() = {
      Json.build(immutable.Map("count" -> count, "minimum" -> minimum, "maximum" -> maximum, "average" -> average)).toString()
    }
  }

  /**
   * Returns a Map[String, Long] of timings. If `reset` is true, the collected timings are
   * cleared, so the next call will return the stats about timings since now.
   */
  def getTimingStats(reset: Boolean): Map[String, TimingStat] = {
    val out = new mutable.HashMap[String, TimingStat]

    for ((key, timing) <- timingMap) {
      val (count, minimum, maximum, average) = timing.getCountMinMaxAvg(reset)
      out += (key -> TimingStat(count, minimum, maximum, average))
    }

    val stats = timingStatsFnList.flatMap(_(reset).toList)
    for ((key, timing) <- stats ++ timingStatsMap) {
      out += (key + TimingStat(timing.count, timing.minimum, timing.maximum, timing.average))
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
