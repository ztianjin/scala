/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala


/** A tuple of 10 elements; the canonical representation of a [[scala.Product10]].
 *
 *  @constructor  Create a new tuple with 10 elements. Note that it is more idiomatic to create a Tuple10 via `(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)`
 *  @param  _1   Element 1 of this Tuple10
 *  @param  _2   Element 2 of this Tuple10
 *  @param  _3   Element 3 of this Tuple10
 *  @param  _4   Element 4 of this Tuple10
 *  @param  _5   Element 5 of this Tuple10
 *  @param  _6   Element 6 of this Tuple10
 *  @param  _7   Element 7 of this Tuple10
 *  @param  _8   Element 8 of this Tuple10
 *  @param  _9   Element 9 of this Tuple10
 *  @param  _10   Element 10 of this Tuple10
 */
case class Tuple10[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10)
  extends Product10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
{  
  
  
  def copyMap[U1, U2, U3, U4, U5, U6, U7, U8, U9, U10](
    f1: T1 => U1 = Function.identity[T1],
    f2: T2 => U2 = Function.identity[T2],
    f3: T3 => U3 = Function.identity[T3],
    f4: T4 => U4 = Function.identity[T4],
    f5: T5 => U5 = Function.identity[T5],
    f6: T6 => U6 = Function.identity[T6],
    f7: T7 => U7 = Function.identity[T7],
    f8: T8 => U8 = Function.identity[T8],
    f9: T9 => U9 = Function.identity[T9],
    f10: T10 => U10 = Function.identity[T10]
  ): Tuple10[U1, U2, U3, U4, U5, U6, U7, U8, U9, U10] = {
    new Tuple10(f1(_1), f2(_2), f3(_3), f4(_4), f5(_5), f6(_6), f7(_7), f8(_8), f9(_9), f10(_10))
  }
    
  override def toString() = "(" + _1 + "," + _2 + "," + _3 + "," + _4 + "," + _5 + "," + _6 + "," + _7 + "," + _8 + "," + _9 + "," + _10 + ")"
}
