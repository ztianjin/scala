/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala


/** A tuple of 1 elements; the canonical representation of a [[scala.Product1]].
 *
 *  @constructor  Create a new tuple with 1 elements.
 *  @param  _1   Element 1 of this Tuple1
 */
case class Tuple1[@specialized(Int, Long, Double) +T1](_1: T1)
  extends Product1[T1]
{  
  
  
  def copyMap[U1](
    f1: T1 => U1 = Function.identity[T1]
  ): Tuple1[U1] = {
    new Tuple1(f1(_1))
  }
    
  override def toString() = "(" + _1 + ")"
}
