/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package funnel

import nsc.util.{ TableDef, JavaStackFrame }
import system.props

trait FunnelUtil {
  private val runtime = Runtime.getRuntime()
  import runtime.{ gc, freeMemory }

  private[funnel] val BILLION        = 1000000000
  private[funnel] val MILLION        = 1000000
  private[funnel] val THOUSAND       = 1000
  private[funnel] val micros         = if (props("file.encoding") startsWith "UTF") "µs" else "us"
  private[funnel] val freeEpsilon    = 1024L
  private[funnel] val freeIterations = 10

  /** Given a property name, looks up the value and, assuming
   *  a string of the form:  prop=abc=def,qq=rr,zz returns
   *    Map(abc -> def, qq -> rr, zz -> "")
   */
  def splitPropertyOnCommas(propName: String): Map[String, String] = {
    props.getOrElse(propName, "") split ',' map { 
      _ split '=' match {
        case Array(k, v)  => (k, v)
        case Array(k)     => (k, "")
      }
    } toMap
  }
  
  def truncateString(maxLen: Int, s: String): String = {
    if (s.length <= maxLen)
      return s
    
    val first = s indexOf '$'
    val last  = s lastIndexOf '$'
    val newlen = first + (s.length - last) + 3
    
    if (first >= 0 && last >= 0 && newlen <= maxLen)
      return (s take first) + "$...$" + (s takeRight (s.length - last))
    
    (s take (maxLen - 3)) + "..."
  }

  def stabilizeFreeMemory(attempts: Int): Long = {
    def loop(last: Long, remaining: Int): Long = {
      if (remaining <= 0) return last

      gc()

      val m = freeMemory()
      if (math.abs(last - m) <= freeEpsilon) math.min(last, m)
      else loop(m, remaining - 1)
    }
    loop(freeMemory(), attempts)
  }

  /** The argument is some code which should free memory (such as 
   *  a call to a mutable map's clear() method) and the change in the
   *  number of bytes visible via freeMemory() will be returned.
   */
  def estimateMemoryUsage(free: => Unit): Long = estimateMemoryUsage(freeIterations, free)
  def estimateMemoryUsage(max: Int, free: => Unit): Long = {
    val before = stabilizeFreeMemory(max)
    free
    val after = stabilizeFreeMemory(max)
    math.max(after - before, 0L)
  }

  /** Strings describing time or space quantities.
   */
  def nanoString(nanos: Long): String = {
    if (nanos > BILLION) nanos / BILLION.toDouble formatted "%.2f s"
    else if (nanos > MILLION) nanos / MILLION.toDouble formatted "%.2f ms"
    else if (nanos > THOUSAND) "%.2f %s".format(nanos / THOUSAND.toDouble, micros)
    else nanos + " ns"
  }
  def bytesString(bytes: Long): String = {
    if (bytes > BILLION) (bytes / BILLION.toDouble) formatted "%.2f GB"
    else if (bytes > MILLION) (bytes / MILLION.toDouble) formatted "%.2f MB"
    else if (bytes > THOUSAND) (bytes / THOUSAND.toDouble) formatted "%.2f kB"
    else bytes + " B"
  }
  def timeSpaceString(nanos: Long, bytes: Long): String = {    
    val quotient = nanos.toDouble / bytes

    if (quotient < THOUSAND) "%.2f %s/kB".format(quotient, micros)
    else if (quotient < MILLION) "%.2f ms/kB".format(quotient / THOUSAND, micros)
    else "%.2f ms/MB".format(quotient / MILLION)
  }
  def percentString(numerator: Long, denominator: Long) = {
    if (denominator == 0) "inf%"
    else "%.2f%%".format(numerator * 100 / denominator.toDouble)
  }
  /** Given a sequence of (T, Int) pairs, creates a map with each distinct
   *  T as a key, its value the sum of all the Ints in (T, Int) pairs.
   */
  def sumPairs[T](pairs: (T, Int)*): Map[T, Int] = {
    val builder = Map.newBuilder[T, Int]
    pairs groupBy (_._1) foreach { case (key, pairs) =>
      builder += ((key, pairs map (_._2) sum))
    }
    builder.result
  }
  
  abstract class KeyValueFormat[K, V](kvs: Seq[(K, V)]) extends TableDef[(K, V)] {
    def maxDefault: Int               = 37
    def maxKey: Int                   = maxDefault
    def maxValue: Int                 = maxDefault
    def keyString(key: K): String     = key.toString
    def valueString(value: V): String = value.toString
      
    >> ("key"   -> (x => truncateString(maxKey, keyString(x._1)))) >+ " -> "
    << ("value" -> (x => truncateString(maxValue, valueString(x._2))))
  }

  def keyValueTable[K, V](maxLen: Int, kvs: Seq[(K, V)]): String = {
    val format = new KeyValueFormat(kvs) { override def maxDefault = maxLen }
    val table  = new format.Table(kvs)
    
    "" + table
  }  
}

object FunnelUtil extends FunnelUtil { }
