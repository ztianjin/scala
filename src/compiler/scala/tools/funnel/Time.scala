/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package funnel

trait Time[K, V] extends HashMapFunnel[K, V] {
  private[funnel] var hitNanos      = 0L
  private[funnel] var missNanos     = 0L
  private[funnel] var overheadNanos = 0L
  private[funnel] def nanosPerHit   = if (hits == 0) 0 else hitNanos / hits
  private[funnel] def nanosPerMiss  = if (misses == 0) 0 else missNanos / misses
  private[funnel] def nanoRatio     = if (nanosPerHit == 0) 0 else nanosPerMiss / nanosPerHit - 1
  private[funnel] def nanoSavings   = (nanosPerMiss - nanosPerHit) * hits - overheadNanos
  
  override def applyHit(key: K): V = {
    val startTime  = System.nanoTime
    val result     = super.applyHit(key)
    val resultTime = System.nanoTime
  
    hitNanos += (resultTime - startTime)
    result
  }
  override def applyMiss(key: K): V = {
    val startTime = System.nanoTime
    val result    = super.applyMiss(key)
    val endTime   = System.nanoTime    

    missNanos += (endTime - startTime)    
    result
  }
  override def updateMemo(key: K, value: V): Unit = {
    val startTime = System.nanoTime
    super.updateMemo(key, value)
    val endTime    = System.nanoTime
    overheadNanos += (endTime - startTime)
  }
}
