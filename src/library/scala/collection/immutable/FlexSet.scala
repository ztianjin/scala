/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: HashSet.scala 16884 2009-01-09 16:52:09Z cunei $

package scala.collection.immutable

import generic._

/** A default implementation of immutable sets.
 *  This is implemented by specialized implementations
 *  `EmptySet`, Set1, ..., Set4, for sets of size up to 4, and
 *  a proxy for an immutable HashSet for larger sets.
 */
trait FlexSet[A] extends Set[A] { self => 
  override def empty = FlexSet.empty
}

/* Factory object for `FlexSet` class */
object FlexSet extends SetFactory[Set] {
  def empty[A]: FlexSet[A] = new EmptySet[A]

  /** An optimized representation for immutable empty sets */
  @serializable
  class EmptySet[A] extends FlexSet[A] {
    override def size: Int = 0
    def contains(elem: A): Boolean = false
    def plus (elem: A): Set[A] = new Set1(elem)
    def minus (elem: A): Set[A] = this
    def elements: Iterator[A] = Iterator.empty
    override def foreach[U](f: A =>  U): Unit = {}
  }
  
  /** An optimized representation for immutable sets of size 1 */
  @serializable
  class Set1[A](elem1: A) extends FlexSet[A] {
    override def size: Int = 1
    def contains(elem: A): Boolean = 
      elem == elem1
    def plus (elem: A): Set[A] = 
      if (contains(elem)) this
      else new Set2(elem1, elem)
    def minus (elem: A): Set[A] = 
      if (elem == elem1) new EmptySet[A] 
      else this
    def elements: Iterator[A] = 
      Iterator(elem1)
    override def foreach[U](f: A =>  U): Unit = {
      f(elem1)
    }
  }

  /** An optimized representation for immutable sets of size 2 */
  @serializable
  class Set2[A](elem1: A, elem2: A) extends FlexSet[A] {
    override def size: Int = 2
    def contains(elem: A): Boolean = 
      elem == elem1 || elem == elem2
    def plus (elem: A): Set[A] = 
      if (contains(elem)) this
      else new Set3(elem1, elem2, elem)
    def minus (elem: A): Set[A] = 
      if (elem == elem1) new Set1(elem2)
      else if (elem == elem2) new Set1(elem1)
      else this
    def elements: Iterator[A] = 
      Iterator(elem1, elem2)
    override def foreach[U](f: A =>  U): Unit = {
      f(elem1); f(elem2)
    }
  }

  /** An optimized representation for immutable sets of size 3 */
  @serializable
  class Set3[A](elem1: A, elem2: A, elem3: A) extends FlexSet[A] {
    override def size: Int = 3
    def contains(elem: A): Boolean = 
      elem == elem1 || elem == elem2 || elem == elem3
    def plus (elem: A): Set[A] = 
      if (contains(elem)) this
      else new Set4(elem1, elem2, elem3, elem)
    def minus (elem: A): Set[A] = 
      if (elem == elem1) new Set2(elem2, elem3)
      else if (elem == elem2) new Set2(elem1, elem3)
      else if (elem == elem3) new Set2(elem1, elem2)
      else this
    def elements: Iterator[A] = 
      Iterator(elem1, elem2, elem3)
    override def foreach[U](f: A =>  U): Unit = {
      f(elem1); f(elem2); f(elem3)
    }
  }

  /** An optimized representation for immutable sets of size 4 */
  @serializable
  class Set4[A](elem1: A, elem2: A, elem3: A, elem4: A) extends FlexSet[A] {
    override def size: Int = 4
    def contains(elem: A): Boolean = 
      elem == elem1 || elem == elem2 || elem == elem3 || elem == elem4
    def plus (elem: A): Set[A] = 
      if (contains(elem)) this
      else new HashSet[A] + (elem1, elem2, elem3, elem4, elem)
    def minus (elem: A): Set[A] = 
      if (elem == elem1) new Set3(elem2, elem3, elem4)
      else if (elem == elem2) new Set3(elem1, elem3, elem4)
      else if (elem == elem3) new Set3(elem1, elem2, elem4)
      else if (elem == elem4) new Set3(elem1, elem2, elem3)
      else this
    def elements: Iterator[A] = 
      Iterator(elem1, elem2, elem3, elem4)
    override def foreach[U](f: A =>  U): Unit = {
      f(elem1); f(elem2); f(elem3); f(elem4)
    }
  }
}