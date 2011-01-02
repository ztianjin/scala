/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package funnel

import scala.system.props
  
// Funnel configuration.
trait Config {
  val isDebug: Boolean
  val label: String

  /** Print a message for all users. */
  def report(msg: String): Unit
  /** Log if debugging is enabled. */
  def log(msg: String): Unit
  /** Summarize whatever this funnel knows in a few lines. */
  def summary(): String
  /** Destructively measure memory usage, clear(), and summary(). */
  def finalSummary(): String
  /** Clear whatever memory this funnel may be hanging onto. */
  def clear(): Unit
  /** Dump whatever this funnel knows to console. */
  def dump(): Unit
}

object Config {
  sealed abstract class Feature(val name: String) {
    def this() = this("")
  }
  case object Debug extends Feature("debug")        // output debugging info
  case object Dump extends Feature("dump")          // dump the memoized contents to console at shutdown
  case object Frequency extends Feature("freq")     // collect key frequency distribution of cache hits
  case object Memoize extends Feature("memo")       // memoize function calls
  case object Origin extends Feature("origin")      // collect method caller stats
  case object Report extends Feature("report")      // install shutdown hook to report collected statistics
  case object Signal extends Feature("signal")      // Register signal handler for dumping state to Console
  case object Time extends Feature("time")          // collect timing stats
  case object Verify extends Feature("verify")      // compare memoized results to re-running the function
  
  def flags = Set[Feature](Debug, Dump, Frequency, Memoize, Origin, Report, Signal, Time, Verify)
  def stats = Set[Feature](Frequency, Origin, Time)
  
  // Mapping from system property names to features.
  val propertyBase = "scala.funnel"
  def propsListed = FunnelUtil.splitPropertyOnCommas(propertyBase)
  
  val booleanFeatures: Map[String, Set[Feature]] = {
    val base = Map[String, Set[Feature]]("stats" -> (stats + Report))
    flags.foldLeft(base)((x, y) => x + ((y.name, Set(y))))
  }

  abstract class ValueFeature[+T] extends Feature { }
  case class LabelV(label: String) extends ValueFeature[String]
  case class SignalV(signal: String) extends ValueFeature[String]
  case class OriginV(origins: Origins) extends ValueFeature[Origins]
  
  case class FeatureSet(features: Set[Feature]) {
    def +(x: Feature): FeatureSet = FeatureSet(features + x)
    def ++(xs: TraversableOnce[Feature]): FeatureSet = FeatureSet(features ++ xs)
    
    def apply(feat: Feature) = features(feat)
    
    def label   = features collectFirst { case LabelV(label) => label }
    def signal  = features collectFirst { case SignalV(signal) => signal }
    def origins = features collectFirst { case OriginV(origins) => origins }
    
    def values: Set[ValueFeature[_]] = features collect { case x: ValueFeature[_] => x }
    def flags: Set[Feature]          = features -- values
    override def toString = features mkString ", "
  }
  object FeatureSet {
    implicit def fromFeatures(xs: Traversable[Feature]): FeatureSet = FeatureSet(xs.toSet)
  }

  def toAbsolute(name: String) = propertyBase + "." + name
  def isSet(name: String)      = (propsListed contains name) || (props contains toAbsolute(name))
  
  // Returns set of Features derived from the system properties.
  def readProps(): FeatureSet = (booleanFeatures filterKeys isSet).values.flatten
}
