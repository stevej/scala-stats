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

import scala.collection.mutable
import net.lag.logging.Logger


/**
 * Log all collected stats as "w3c-style" lines to a java logger at a regular interval.
 */
class W3CStatsLogger(val logger: Logger, val frequencyInSeconds: Int, includeJvmStats: Boolean) extends BackgroundProcess("W3CStatsLogger") {
  def this(logger: Logger, frequencyInSeconds: Int) = this(logger, frequencyInSeconds, true)

  val reporter = new W3CReporter(logger)
  var previousCounterStats = new mutable.HashMap[String, Long]

  def runLoop() {
    Thread.sleep(frequencyInSeconds * 1000)
    logStats()
  }

  def logStats() {
    val report = new mutable.HashMap[String, Any]
    if (includeJvmStats) {
      Stats.getJvmStats() foreach { case (key, value) => report("jvm_" + key) = value }
    }
    Stats.getCounterStats(false) foreach { case (key, value) =>
      report(key) = (value - previousCounterStats.get(key).getOrElse(0L))
      previousCounterStats(key) = value
    }
    Stats.getGaugeStats(false) foreach { case (key, value) => report(key) = value }
    Stats.getTimingStats(false) foreach { case (key, timing) =>
      report(key + "_count") = timing.count
      report(key + "_min") = timing.minimum
      report(key + "_max") = timing.maximum
      report(key + "_sum") = timing.sum
      report(key + "_sumsq") = timing.sumSquares
      report(key + "_avg") = timing.average
      report(key + "_std") = timing.standardDeviation
    }

    reporter.report(report)
  }
}
