/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package funnel

import nsc.util.{ JavaStackFrame, FrameContext }

/** A class for recording from whence a certain call originated.
 *  This trick is accomplished by having an implicit value of type Caller
 *  available, and when said implicit is invoked, a stack trace is recorded
 *  and trimmed to start with the original caller.
 */
class Caller(val stack: StackSlice) extends JavaStackFrame(stack.head) {  
  def top       = JavaStackFrame(stack.head)
  def show()    = context() foreach println
  def frames()  = stack map (x => JavaStackFrame(x))
  def context() = stack map (x => FrameContext(x))
  
  def drop(numFrames: Int): Caller = new Caller(stack drop numFrames)
  
  override def toString = locationString
}
trait LowPriorityCaller {
  self: Caller.type =>

  private val ThisFile  = "Caller.scala"
  private val ThisClass = this.getClass.getName
  private def isLocal(ste: StackTraceElement) = ste.getFileName == ThisFile || ste.getClassName == ThisClass

  def trace() = (new Throwable).getStackTrace() dropWhile isLocal
  implicit def reifyCallerStack: Caller = new Caller(trace())
}
object Caller extends LowPriorityCaller {
  
}
