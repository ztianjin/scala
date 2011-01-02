/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package funnel

class Report(memo: Memo[_, _]) {
  import memo._
  
  private def lineup(s1: String, s2: String, s3: String, s4: String) = {
    "%10s: %18s | %-18s | %s".format(s1, s2, s3, s4)
  }
  private def ifFunnelTime[T](f: Time[_, _] => T): Option[T] = memo match {
    case time: Time[_, _] => Some(f(time))
    case _                => None
  }
  private val _nanoSavings = ifFunnelTime(_.nanoSavings) getOrElse 0L
  private val _nanoRatio   = ifFunnelTime(_.nanoRatio) getOrElse 0L
  private val header       = "\n>> " + label + " (" + size + " keys)"
  private val hitRate      = lineup("Hit rate", hits + " hits", calls + " calls", percentString(hits, calls))
  private val timing       = ifFunnelTime { time =>
    import time._
    lineup("Timing", nanoString(nanosPerHit) + "/hit", nanoString(nanosPerMiss) + "/miss", "1:" + _nanoRatio)
  }

  def apply(clearMemory: => Long): String = {
    val msgs = allMessages()

    // destructively measure memory utilization
    val bytes    = clearMemory
    
    // post-clearing code: now we have a value for bytes
    val velocity = {
      if (bytes <= 0L || _nanoSavings <= 0L) "Velocity: N/A"
      else lineup("Velocity",
        nanoString(_nanoSavings) + " saved",
        bytesString(bytes) + " memory",
        timeSpaceString(_nanoSavings,
        bytes)
      )
    }

    List(header, hitRate, velocity) ++ timing.toList ++ msgs mkString "\n"
  }
}
