/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package funnel

import scala.collection.{ mutable, immutable }

abstract class HashMapFunnel[K, V] extends Memo[K, V] {
  private[funnel] val map    = new mutable.HashMap[K, V]
  private[funnel] def applyHit(key: K): V = {
    hits += 1
    map(key)
  }
  private[funnel] def applyMiss(key: K): V = {
    misses += 1
    f(key)
  }
  private[funnel] def updateMemo(key: K, value: V): Unit = {
    this += ((key, value))
  }

  def size                = map.size
  def toList              = map.toList filterNot (x => x == null || x._1 == null || x._2 == null)
  def isDefinedAt(key: K) = map isDefinedAt key
  // def memoize(key: K)     = map.getOrElseUpdate(key, f(key))
  
  def +=(kv: (K, V)): this.type = { map(kv._1) = kv._2 ; this }
  def -=(key: K): this.type = { map -= key ; this }
  
  def clear()         = map.clear()
  def dump()          = report(keyValueTable(37, toList sortBy (_._1.toString)))
}

