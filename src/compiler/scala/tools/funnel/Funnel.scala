/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package funnel

import scala.collection.{ mutable, immutable }
import Config._
import FunnelManager._

class Funnel[K, V](val f: K => V, features: FeatureSet, caller: Caller) extends {
  val label       = features.label getOrElse caller.locationString
  val origins     = features.origins orElse ({
    if (features(Origin)) Some(Origins(caller))
    else None
  }) orNull
  val signal      = features.signal
  val isDebug     = features(Debug)
  val isDump      = features(Dump)
  val isMemoize   = features(Memoize)
  val isReport    = features(Report)
  val freqStats   = features(Frequency)
  val timingStats = features(Time)
  val originStats = origins != null
} with HashMapFunnel[K, V] with Frequency[K, V] {
  self =>
  
  private def originsInit() {
    addMessage {
      if (origins.isEmpty) Nil
      else Seq("Frequently seen frames.\n", origins.topFrames())
    }
    // memo.addMessage(Seq("Top call sites.\n", memo.origins.topCallers()))
    addMessage {
      if (origins.isEmpty) Nil
      else Seq("Top origins (depth = " + origins.depth + ").\n", origins.topKeys())
    }
  }
  
  locally {
    if (freqStats)    frequencyInit()
    if (originStats)  originsInit()
    if (isReport)     reportAtShutdown(this)
      
    log(toString)
  }
    
  def apply(key: K): V = {
    if (freqStats)
      freq(key) += 1
    if (originStats)
      origins.record()
    
    if (this isDefinedAt key) {
      val result = applyHit(key)
      if (isMemoize) result
      else f(key)
    }
    else {
      val result = applyMiss(key)
      updateMemo(key, result)
      result
    }
  }
  override def toString = "%s (%s)".format(label, features)
}


object Funnel {
  import Config._

  private def propsAnd(extras: Feature*): FeatureSet =
    extras.foldLeft(readProps())(_ + _)
  
  class FunnelTimed[K, V](f: K => V, features: FeatureSet, caller: Caller)
              extends Funnel(f, features, caller)
                 with Time[K, V] { }
  
  def memoize[K, V](f: K => V)(implicit caller: Caller): Funnel[K, V] =
    create(f, propsAnd(Memoize))(caller)
  
  def originize[K, V](f: K => V)(implicit caller: Caller): Funnel[K, V] =
    create(f, propsAnd(Origin))(caller)
    
  def timed[K, V](f: K => V)(implicit caller: Caller): Funnel[K, V] =
    create(f, propsAnd(Time))(caller)

  def timed[K, V](f: K => V, features: FeatureSet)(implicit caller: Caller): Funnel[K, V] =
    new FunnelTimed(f, features + Time, caller) { }
  
  /** **/
  
  def apply[K, V](f: K => V)(implicit caller: Caller): Funnel[K, V] =
    create(f, propsAnd())(caller)
  
  def create[K, V](f: K => V, features: FeatureSet)(implicit caller: Caller): Funnel[K, V] = {
    if (features(Time)) timed(f, features)(caller)
    else new Funnel(f, features, caller) { }
  }
  // def originize[T1, T2, V](f: (T1, T2) => V)(implicit caller: Caller): (T1, T2) => V =
  //   Function.untupled(Funnel.originize(f.tupled)(caller))  
}
