/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package funnel

import scala.collection.{ mutable, immutable }

class Track(val track: String) {
  type Args
  type Result
  
  case class Racer(name: String, fn: Args => Result) {
    private var totalNanos: Long = 0
  }

  // private var laps: Int = 0
  // def lap(args: Args): Result = { }
  
  final def timed(body: => Result): (Long, Result) = {
    val t1 = System.nanoTime
    val result = body
    val t2 = System.nanoTime
    
    (t2 - t1, result)
  }
}

abstract class Racer[T] {
  type Args
  def raceManager: Racer.Manager
  def racerName: String
  def f1(x: Args): T
  def f2(x: Args): T
  
  def f1Name: String = "f1 (old)"
  def f2Name: String = "f2 (new)"

  final def timed(body: => T): (Long, T) = {
    val t1 = System.currentTimeMillis()
    val result = body
    val t2 = System.currentTimeMillis()
    
    (t2 - t1, result)
  }
  def race(args: Args): T = {
    val inOrder = raceManager.first(this)
    val (t1, r1) = timed(if (inOrder) f1(args) else f2(args))
    val (t2, r2) = timed(if (inOrder) f2(args) else f1(args))
    
    assert(r1 == r2, "" + r1 + " " + r2)
    raceManager.record(this, t1, t2)
    r1
  }
  override def equals(other: Any) = other match {
    case x: Racer[_]  => racerName == x.racerName
    case _            => false
  }
  override def hashCode = racerName.hashCode
  override def toString = "Racer '" + racerName + "' managed by " + raceManager
}

object Racer {
  class Manager {
    private class RaceTrack {
      var count = 0
      var f1Time = 0L
      var f2Time = 0L
    }
    private val races = new mutable.HashMap[Racer[_], RaceTrack] withDefault (_ => new RaceTrack)

    private def showTimes() {
      if (races.isEmpty)
        Console.println("No races to show!")

      for ((racer, track) <- races) {
        import track._

        Console println """
          |Total time used in %d races of %s:
          |  %s: %d ms
          |  %s: %d ms
        """.stripMargin.trim.format(count, racer.racerName, racer.f1Name, f1Time, racer.f2Name, f2Time)
      }
    }
    def first(racer: Racer[_]) = races(racer).count % 2 == 0

    def record(racer: Racer[_], f1Time: Long, f2Time: Long): Unit = {
      val track = races(racer)
      track.count += 1
      track.f1Time += f1Time
      track.f2Time += f2Time
      races(racer) = track
    }

    system addShutdownHook showTimes()
  }
}

  // object plausiblyRacer extends Racer[Boolean] {
  //   type Args = (MethodType, Type)
  //   val raceManager = raceMgr
  //   val racerName = "isPlausiblyCompatible"
  //   
  //   def f1(mtpt: (MethodType, Type)): Boolean = {
  //     val (mt @ MethodType(params, restpe), TypeRef(pre, sym, args)) = mtpt
  //     val l = args.length - 1
  //     l == params.length &&
  //     sym == FunctionClass(l) && {
  //       var curargs = args
  //       var curparams = params
  //       while (curparams.nonEmpty) {
  //         if (!isPlausiblySubType(curargs.head, curparams.head.tpe))
  //           return false
  //         curargs = curargs.tail
  //         curparams = curparams.tail
  //       }
  //       isPlausiblySubType(restpe, curargs.head)
  //     }
  //   }
  //   def f2(mtpt: (MethodType, Type)): Boolean = {
  //     val (mt @ MethodType(params, restpe), TypeRef(pre, sym, args)) = mtpt
  //     val len = args.length - 1
  //     params.lengthCompare(len) == 0 &&
  //     sym == FunctionClass(len) && {
  //       val ps = mt.paramTypes.iterator
  //       val as = args.iterator
  //       while (ps.hasNext && as.hasNext) {
  //         if (!isPlausiblySubType(as.next, ps.next))
  //           return false
  //       }
  //       ps.isEmpty && as.hasNext && {
  //         val lastArg = as.next
  //         as.isEmpty && isPlausiblySubType(restpe, lastArg)
  //       }
  //     }
  //   }
  // }
