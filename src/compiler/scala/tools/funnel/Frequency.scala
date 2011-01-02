/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package funnel

import scala.collection.{ mutable, immutable }
import nsc.util.TableDef

trait Frequency[K, V] extends HashMapFunnel[K, V] {
  private[funnel] val freq = new mutable.HashMap[K, Int] withDefaultValue 0
  private[funnel] def frequencyInit() = {
    addMessage(
      "Top cache keys by incidence.\n" +:
      (byPopularity(10) map { case (num, key) => "%5d: %s\n".format(num, key) })
    )
    addMessage(
      Seq(
        "Frequency distribution for cache hits, excluding fill.\n",
        new FrequencyFormat.Table(frequencyPartition(10)) + "\n"
      )
    )
  }

  private def keyCount               = freq.keys.size
  private def callCount              = freq.values.toList.sum
  private def callsPerKey            = callCount / keyCount.toDouble
  private def byPopularity(num: Int) = freq.toList map (_.swap) sortBy (-_._1) take num
  
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
  
  override def clear() = { super.clear() ; freq.clear() }
}
