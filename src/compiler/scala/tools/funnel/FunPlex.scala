/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package funnel

/** When you have too many multis, you multiplex.
 *  When you have too much fun, you funplex.
 */
abstract class Funplex[K, V](val f: K => V, val arity: Int) {
  
}

class Funplex1[K1, V](f: K1 => V) extends Funplex(f, 1)
class Funplex2[K1, K2, V](f: (K1, K2) => V) extends Funplex(f.tupled, 2)
class Funplex3[K1, K2, K3, V](f: (K1, K2, K3) => V) extends Funplex(f.tupled, 3)
class Funplex4[K1, K2, K3, K4, V](f: (K1, K2, K3, K4) => V) extends Funplex(f.tupled, 4)
class Funplex5[K1, K2, K3, K4, K5, V](f: (K1, K2, K3, K4, K5) => V) extends Funplex(f.tupled, 5)

object Funplex {
  implicit def f1toFunplex[K1, V](f: K1 => V) = new Funplex1(f)
  implicit def f2toFunplex[K1, K2, V](f: (K1, K2) => V) = new Funplex2(f)
  implicit def f3toFunplex[K1, K2, K3, V](f: (K1, K2, K3) => V) = new Funplex3(f)
  implicit def f4toFunplex[K1, K2, K3, K4, V](f: (K1, K2, K3, K4) => V) = new Funplex4(f)
  implicit def f5toFunplex[K1, K2, K3, K4, K5, V](f: (K1, K2, K3, K4, K5) => V) = new Funplex5(f)
}
