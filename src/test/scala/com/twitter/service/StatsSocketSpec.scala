/** Copyright 2009 Twitter, Inc. */
package com.twitter.service

import net.lag.extensions._
import org.specs._
import scala.collection.immutable
import java.net.{Socket, SocketException}
import java.io.{BufferedReader, InputStreamReader, PrintWriter}


object StatsSocketSpec extends Specification {
  def fn(cmd: String): String = cmd match {
    case "stats text" => "foo: 1\n"
    case "stats json" => "{\"foo\": 1}\n"
    case cmd => "error unknown command: " + cmd + "\n"
  }

  "StatsSocket" should {
    var listener: StatsSocketListener = null
    var server: Thread = null
    var clientSocket: Socket = null
    var out: PrintWriter = null
    var in: BufferedReader = null

    doBefore {
      if (listener != null) { listener.stop }
      if (server != null) { server.stop }

      listener = new StatsSocketListener(43210, fn)
      server = new Thread(listener)
      server.start

      clientSocket = try {
        new Socket("localhost", 43210)
      } catch {
        case e: Exception => fail("unable to connect to port 43210"); null
      }
      out = new PrintWriter(clientSocket.getOutputStream, true)
      in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream))
    }

    "return text content correctly with multiple requests" >> {
      out.println("stats text")
      out.flush()
      in.readLine() mustEqual "foo: 1"

      out.println("stats text")
      out.flush()
      in.readLine() mustEqual ""
      in.readLine() mustEqual "foo: 1"
    }

    "return json content correctly with multiple requests" >> {
      out.println("stats json")
      out.flush()
      in.readLine() mustEqual "{\"foo\": 1}"

      out.println("stats json")
      out.flush()
      in.readLine() mustEqual ""
      in.readLine() mustEqual "{\"foo\": 1}"
    }
  }
}
