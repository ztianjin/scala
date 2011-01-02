/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package funnel

import scala.system.props
import scala.collection.{ mutable, immutable }
import nsc.util.JavaStackFrame

trait FunnelReporting {
  self: Memo[_, _] =>
  
  private def newSummary = new Report(this)

  def summary(): String = newSummary(0L)
  def finalSummary(): String = newSummary(estimateMemoryUsage(clear()))
  def allMessages() = messages map (f => f())

  def report(msg: String): Unit = Console println msg
  def log(msg: String)          = if (isDebug) report(msg)
  
  // Recording messages for later display
  private[funnel] var messages: List[() => String] = Nil
  private[funnel] def assembleMessage(segments: Seq[String]): String = {
    if (segments forall (_ == "")) ""
    else "\n>> [%s]\n".format(label) + segments.mkString("")
  }
  private[funnel] def addMessage(msgs: => Seq[String]): Unit =
    messages :+= (() => assembleMessage(msgs))
}

/** Function memoization.
 */
trait Memo[K, V] extends PartialFunction[K, V] with Config with FunnelUtil with FunnelReporting {
  def f: K => V
  def apply(key: K): V
  def isDefinedAt(key: K): Boolean
  def +=(kv: (K, V)): this.type

  // Implementations
  def ++=(xs: TraversableOnce[(K, V)]): this.type = { xs foreach += ; this }
  def ++=(xs: (K, V)*): this.type = { xs foreach += ; this }
  
  // Cheap statistics
  def size: Int
  private[funnel] var hits: Int = 0
  private[funnel] var misses: Int = 0
  private[funnel] def calls: Int = hits + misses

  override def toString = summary()
}
