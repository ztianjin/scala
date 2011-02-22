/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala


/** A tuple of 13 elements; the canonical representation of a [[scala.Product13]].
 *
 *  @constructor  Create a new tuple with 13 elements. Note that it is more idiomatic to create a Tuple13 via `(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)`
 *  @param  _1   Element 1 of this Tuple13
 *  @param  _2   Element 2 of this Tuple13
 *  @param  _3   Element 3 of this Tuple13
 *  @param  _4   Element 4 of this Tuple13
 *  @param  _5   Element 5 of this Tuple13
 *  @param  _6   Element 6 of this Tuple13
 *  @param  _7   Element 7 of this Tuple13
 *  @param  _8   Element 8 of this Tuple13
 *  @param  _9   Element 9 of this Tuple13
 *  @param  _10   Element 10 of this Tuple13
 *  @param  _11   Element 11 of this Tuple13
 *  @param  _12   Element 12 of this Tuple13
 *  @param  _13   Element 13 of this Tuple13
 */
case class Tuple13[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13)
  extends Product13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
{  
  
  
  def copyMap[U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13](
    f1: T1 => U1 = Function.identity[T1],
    f2: T2 => U2 = Function.identity[T2],
    f3: T3 => U3 = Function.identity[T3],
    f4: T4 => U4 = Function.identity[T4],
    f5: T5 => U5 = Function.identity[T5],
    f6: T6 => U6 = Function.identity[T6],
    f7: T7 => U7 = Function.identity[T7],
    f8: T8 => U8 = Function.identity[T8],
    f9: T9 => U9 = Function.identity[T9],
    f10: T10 => U10 = Function.identity[T10],
    f11: T11 => U11 = Function.identity[T11],
    f12: T12 => U12 = Function.identity[T12],
    f13: T13 => U13 = Function.identity[T13]
  ): Tuple13[U1, U2, U3, U4, U5, U6, U7, U8, U9, U10, U11, U12, U13] = {
    new Tuple13(f1(_1), f2(_2), f3(_3), f4(_4), f5(_5), f6(_6), f7(_7), f8(_8), f9(_9), f10(_10), f11(_11), f12(_12), f13(_13))
  }
    
  override def toString() = "(" + _1 + "," + _2 + "," + _3 + "," + _4 + "," + _5 + "," + _6 +
    "," + _7 + "," + _8 + "," + _9 + "," + _10 + "," + _11 + "," + _12 + "," + _13 + ")"
}
