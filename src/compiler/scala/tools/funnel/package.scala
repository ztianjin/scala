/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools

import scala.system.props
import scala.collection.{ mutable, immutable }

package object funnel {
  private val thisClassName = this.getClass.getName
  
  type StackSlice = Array[StackTraceElement]
  type StackSeq   = IndexedSeq[StackTraceElement]

  /** A sequence of stack frames starting with the one corresponding
   *  to the caller of this function.
   */
  def callerStack(dropFrames: Int = 0): Array[StackTraceElement] = {
    new Throwable getStackTrace() dropWhile (_.getClassName == thisClassName) drop dropFrames
  }
  /** A string representing the file and line number arrived at by dropping stack
   *  frames until arriving at the caller, then dropping `dropFrames` more.
   */
  def callerLocation(dropFrames: Int = 0): String = {
    callerStack(dropFrames).headOption map (s => s.getFileName + ":" + s.getLineNumber) getOrElse "<unknown>"
  }
  
  /** Convenience. */
  def erasure[T: ClassManifest] : Class[_] = classManifest[T].erasure
}
