/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package funnel

import scala.collection.{ mutable, immutable }
import nsc.util.{ TableDef, JavaStackFrame }
import scala.tools.util.Signallable

/** A debugging class for logging from whence a method is being called.
 *  Say you wanted to discover who was calling phase_= in SymbolTable.
 *  You could do this:
 *  
 *  {{{
 *    private lazy val origins = Origins[SymbolTable]("phase_=")
 *    // Commented out original enclosed for contrast
 *    // final def phase_=(p: Phase): Unit = {
 *    final def phase_=(p: Phase): Unit = origins {
 *  }}}
 *
 *  And that's it.  When the JVM exits it would issue a report something like this:
 {{{
 >> Origins scala.tools.nsc.symtab.SymbolTable.phase_= logged 145585 calls from 51 distinguished sources.

   71114   scala.tools.nsc.symtab.Symbols$Symbol.unsafeTypeParams(Symbols.scala:862)
   16584   scala.tools.nsc.symtab.Symbols$Symbol.rawInfo(Symbols.scala:757)
   15411   scala.tools.nsc.symtab.Symbols$Symbol.unsafeTypeParams(Symbols.scala:869)
   11507   scala.tools.nsc.symtab.Symbols$Symbol.rawInfo(Symbols.scala:770)
   10285   scala.tools.nsc.symtab.Symbols$Symbol.unsafeTypeParams(Symbols.scala:864)
    6860   scala.tools.nsc.transform.SpecializeTypes.specializedTypeVars(SpecializeTypes.scala:304)
    ...
 }}}
 *
 */

abstract class Origins extends OriginsConfig with FunnelUtil {  
  def frameOffset = 2    // XXX
  def label = origination.toString

  def matchesOrigination(ste: StackTraceElement) =
    ste.getFileName == origination.fileName || ste.getClassName == origination.className

  def trim(stack: StackSlice): StackSlice =
    stack dropWhile (x => !matchesOrigination(x)) drop frameOffset take depth

  def frameString(x: StackTraceElement) = JavaStackFrame(x).locationString
  def keyString(key: Key)               = frameString(caller(key))
  def sameCaller(k1: Key, k2: Key)      = caller(k1) == caller(k2)

  def record(): Unit = {
    val stack = trim(callerStack())
    originCounts(newKey(stack)) += 1
    stack foreach (el => frameCounts(el) += 1)
  }
  def apply[T](value: T): T = {
    record()
    value
  }
  def isEmpty = originCounts.isEmpty

  /** Top N to show for charts. */
  def originsToShow = 10
  
  // Record each call under class-defined key
  private val originCounts = new mutable.HashMap[Key, Int] withDefaultValue 0
  private def calls = originCounts.values.sum

  // Flatten out all stack traces and count the first N frames as equally significant.
  private val frameCounts = new mutable.HashMap[StackTraceElement, Int] withDefaultValue 0

  // The set of stack frames which are at the top of at least one recorded trace.
  private def callSites = originCounts.keys map caller toSet
  
  // Collapsing all traces down to their top frame, the hit count for each of those
  private def callSiteCounts: Map[StackTraceElement, Int] =
    sumPairs(originCounts.toList collect { case (key, num) => caller(key) -> num } : _*)
  
  private def freqsort[K](m: Traversable[(K, Int)]) = m.toList map (_.swap) sortBy (-_._1)
  private def originsSorted   = freqsort(originCounts)
  private def callSitesSorted = freqsort(callSiteCounts)
  private def framesSorted    = freqsort(frameCounts)

  // keyValueTable(100, originsSorted take num)
  def topKeys(num: Int = originsToShow)    = new OriginsFormat.Table(originsSorted take num) + "\n"
  def topCallers(num: Int = originsToShow) = new StackFrameFreqFormat.Table(callSitesSorted take num) + "\n"
  def topFrames(num: Int = originsToShow)  = new StackFrameFreqFormat.Table(framesSorted take num) + "\n"

  private object OriginsFormat extends TableDef[(Int, Key)] {
    >> ("calls" -> (x => x._1)) >+ "  "
    << ("origin" -> (x => keyString(x._2)))
  }
  private object StackFrameFreqFormat extends TableDef[(Int, StackTraceElement)] {
    >> ("frame" -> (x => frameString(x._2)))
    << ("num"   -> (_._1))
  }

  def clear(): Unit   = { originCounts.clear() ; frameCounts.clear() }
  def dump(): Unit    = { originsSorted foreach println }

  private val signallable = Signallable("Dump contents of Origins '%s' to console.".format(label))(dump())
  override def toString = "Origins '%s': %d calls / signal with %s".format(label, calls, signallable.signal)
}

trait OriginsConfig {
  /** The type of the map key. */
  type Key
  /** How many stack frames to look at when distinguishing between calls. */
  def depth: Int
  /** The inflection point beyond which we are looking at actual callers. */
  def origination: Caller
  /** The number of additional frames to drop beyond origination. */
  def frameOffset: Int
  /** Create a new map key from the current stack trace. */
  def newKey(stack: StackSlice): Key
  /** Recover the caller frame from a Key. */
  def caller(key: Key): StackTraceElement
}

object Origins {
  val DefaultStackDepth = 5
  private val counters = new mutable.HashSet[Origins]
  
  trait Parameterized[K] extends Origins {
    type Key = K
  }
  class CallerOrigins(val origination: Caller) extends Parameterized[StackTraceElement] {
    def depth = 1
    def newKey(stack: StackSlice) = stack.head
    def caller(key: Key)          = key
  }
  class CallerWithThreadOrigins(val origination: Caller) extends Parameterized[(StackTraceElement, Thread)] {    
    def depth = 1
    def newKey(stack: StackSlice)    = (stack.head, Thread.currentThread)
    def caller(key: Key)             = key._1
    override def keyString(key: Key) = {
      val (elem, t) = key
      "(%s, %s)".format(frameString(elem), t.getId)
    }
  }
  class StackSeqOrigins(val origination: Caller, val depth: Int) extends Parameterized[StackSeq] {
    private object MultiFrameFormat extends TableDef[(Int, Key)] {
      >> ("calls" -> (x => x._1)) >+ "  "
      1 to depth foreach { d => 
        << (("T-" + d) -> (x => frameStrings(x._2)(d - 1)))
      }
    }
    private def frameFile(el: StackTraceElement) = el.getFileName stripSuffix ".scala"
    private def frameStrings(key: Key): IndexedSeq[String] = {
      val seed = frameFile(key.head)
      val (filenames, _) =
        (key drop 1).foldLeft((List(seed), seed)) { case ((res, last), el) =>
          val file = frameFile(el)
          if (last == file) (res :+ "<- ", last)
          else (res :+ file, file)
        }
      
      (filenames, key).zipped map { (file, el) =>
        val s = if (file == "") " <- " else (file + ":")
        s + el.getLineNumber
      } toIndexedSeq
    }

    def newKey(stack: StackSlice)    = stack.toIndexedSeq
    def caller(key: Key)             = key.head
    override def topKeys(num: Int = originsToShow)    = new MultiFrameFormat.Table(originsSorted take num) + "\n"
    override def keyString(key: Key) = frameStrings(key) mkString " <- "
  }
  class StackSeqWithThreadOrigins(val origination: Caller, val depth: Int) extends Parameterized[(StackSeq, Thread)] {
    def newKey(stack: StackSlice)    = (stack.toIndexedSeq, Thread.currentThread)
    def caller(key: Key)             = key._1.head
    override def keyString(key: Key) = "(%s, %s)".format(super.keyString(key), key._2.getName)
  }

  def apply(implicit orig: Caller): Origins = apply(DefaultStackDepth)
  def apply(depth: Int)(implicit orig: Caller): Origins = apply(depth, false)
  def apply(depth: Int, thread: Boolean)(implicit orig: Caller): Origins = (depth, thread) match {
    case (0, false)   => new CallerOrigins(orig)
    case (0, true)    => new CallerWithThreadOrigins(orig)
    case (_, false)   => new StackSeqOrigins(orig, depth)
    case (_, true)    => new StackSeqWithThreadOrigins(orig, depth)
  }
}

