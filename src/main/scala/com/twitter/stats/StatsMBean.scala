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

import java.lang.management.ManagementFactory
import javax.{management => jmx}


object StatsMBean {
  def apply(packageName: String): Unit = apply(packageName, ManagementFactory.getPlatformMBeanServer())

  def apply(packageName: String, mbeanServer: jmx.MBeanServer): Unit = {
    mbeanServer.registerMBean(new StatsMBean(), new jmx.ObjectName(packageName + ":type=Stats"))
  }
}


class StatsMBean extends jmx.DynamicMBean {
  def getMBeanInfo() = {
    new jmx.MBeanInfo("com.twitter.service.Stats", "running statistics", getAttributeInfo(),
      null, null, null, new jmx.ImmutableDescriptor("immutableInfo=false"))
  }

  def getAttribute(name: String): AnyRef = {
    val segments = name.split("_", 2)
    segments(0) match {
      case "counter" =>
        Stats.getCounterStats()(segments(1)).asInstanceOf[java.lang.Long]
      case "timing" =>
        val prefix = segments(1).split("_", 2)
        val timing = Stats.getTimingStats(false)(prefix(1))
        val x = prefix(0) match {
          case "min" => timing.minimum
          case "max" => timing.maximum
          case "count" => timing.count
          case "average" => timing.average
        }
        x.asInstanceOf[java.lang.Integer]
      case "gauge" =>
        Stats.getGaugeStats(false)(segments(1)).asInstanceOf[java.lang.Double]
    }
  }

  def getAttributes(names: Array[String]): jmx.AttributeList = {
    val rv = new jmx.AttributeList
    for (name <- names) rv.add(new jmx.Attribute(name, getAttribute(name)))
    rv
  }

  def invoke(actionName: String, params: Array[Object], signature: Array[String]): AnyRef = throw new IllegalStateException()

  def setAttribute(attr: jmx.Attribute): Unit = throw new IllegalStateException()

  def setAttributes(attrs: jmx.AttributeList): jmx.AttributeList = throw new IllegalStateException()

  private def getAttributeInfo(): Array[jmx.MBeanAttributeInfo] = {
    (Stats.getCounterStats().keys.map { name =>
      List(new jmx.MBeanAttributeInfo("counter_" + name, "java.lang.Long", "counter", true, false, false))
    } ++ Stats.getTimingStats(false).keys.map { name =>
      List("min", "max", "average", "count") map { prefix =>
        new jmx.MBeanAttributeInfo("timing_" + prefix + "_" + name, "java.lang.Integer", "timing", true, false, false)
      }
    } ++ Stats.getGaugeStats(false).keys.map { name =>
      List(new jmx.MBeanAttributeInfo("gauge_" + name, "java.lang.Long", "gauge", true, false, false))
    }).toList.flatten[jmx.MBeanAttributeInfo].toArray
  }
}