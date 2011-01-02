/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package funnel

import scala.system.props
import scala.collection.{ mutable, immutable }

trait ObliviousFunnel extends MemoUtil {
  def action(stack: StackSlice): Unit
}


trait Funnel[K, V] extends (K => V) with MemoUtil {
  val f: K => V
  val label: String
  val config: Config
  def apply(key: K): V
  
  protected def report(msg: String): Unit = Console println msg
  protected def log(msg: String) = if (config.isDebug) report(msg)
  protected var messageFns: List[() => String] = List(() => "Funnel " + label + ":")
  
  def summary() = messageFns map (f => f()) mkString ""
  override def toString = summary()
}

object Funnel {
  trait Memoize[K, V] extends PartialFunction[K, V] {
    self: Funnel[K, V] =>
    
    def +=(kv: (K, V)): this.type
    def ++=(xs: TraversableOnce[(K, V)]): this.type = { xs foreach += ; this }
    def ++=(xs: (K, V)*): this.type = { xs foreach += ; this }

    def memoize(key: K): V
    def size: Int
    def isDefinedAt(key: K): Boolean
    def clear(): Unit
  }
  
  trait MemoizeHashMap[K, V] extends Memoize[K, V] {
    self: Funnel[K, V] =>

    protected val map = new mutable.HashMap[K, V]

    def +=(kv: (K, V)): this.type = { map += kv ; this }

    def memoize(key: K)     = map.getOrElseUpdate(key, f(key))
    def size                = map.size
    def isDefinedAt(key: K) = map isDefinedAt key
    def clear()             = map.clear()
  }
  
  trait MemoStats[K, V] extends Memoize[K, V] {
    self: Funnel[K, V] =>
    
    def count(key: K) = {
      if (self isDefinedAt key) hits += 1
      else misses += 1
      
      freq(key) += 1
    }
    
    private val freq         = new mutable.HashMap[K, Int] withDefaultValue 0
    protected var hits       = 0L
    protected var misses     = 0L
    protected def calls      = hits + misses
    private def keyCount     = freq.keys.size
    private def callCount    = freq.values.toList.sum
    private def callsPerKey  = callCount / keyCount.toDouble
    private def byPopularity = freq.toList sortBy (-_._2)
    
    private def frequencyPartition(max: Int): List[Freq] = {
      val keys = new Array[Int](max + 1)
      val calls = new Array[Int](max + 1)
      for ((key, count) <- freq) {
        if (count < max) {
          keys(count) += 1
          calls(count) += count
        }
        else {
          keys(max) += 1
          calls(max) += count
        }
      }
      (1 to max).toList map { idx =>
        val descr = if (idx == max) (idx - 1) + "+" else "" + (idx - 1)
        Freq(idx - 1, keys(idx), calls(idx), descr)
      }
    }
    private case class Freq(rank: Int, keys: Int, calls: Int, descr: String) { }
    private object FrequencyFormat extends TableDef[Freq] {
      >> ("hits"  -> (_.descr)) >+ "  "
      << ("keys"  -> (_.keys)) >+ "  "
      >> ("% of keys" -> ({ x => percentString(x.keys, keyCount) })) >+ "  "
      >> ("% of calls" -> ({ x => percentString(x.calls, callCount) }))
    }
    
    private def baseMessage() = 
      " %d calls, %d distinct keys.".format(calls, size)

    private def hitsMessage() =
      "  Hit rate: %d / %d (%s)\n".format(hits, calls, percentString(hits, calls))

    private def popularMessage() = {
      val strs = byPopularity take 10 map {
        case (key, count) => "  %4d: %s".format(count, key)
      }
      strs.mkString("Keys with high hit rates:\n", "\n", "")
    }
    private def freqMessage() = (
      "Keys distributed by number of cache hits after fill:\n" +
      new FrequencyFormat.Table(frequencyPartition(10))
    )

    locally {
      messageFns ++= List[() => String](baseMessage, hitsMessage, popularMessage, freqMessage)
    }
  }
}
