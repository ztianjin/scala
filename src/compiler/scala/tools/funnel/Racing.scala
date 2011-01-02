/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package funnel

class Racing[T](val trackName: String, check: (T, T) => Unit) {
  import scala.util.Random.shuffle
  
  // dependency hell -- delete this
  val BILLION        = 1000000000
  val MILLION        = 1000000
  val THOUSAND       = 1000
  val micros         = if (system.props("file.encoding") startsWith "UTF") "µs" else "us"
  def nanoString(nanos: Long): String = {
    if (nanos > BILLION) nanos / BILLION.toDouble formatted "%.2f s"
    else if (nanos > MILLION) nanos / MILLION.toDouble formatted "%.2f ms"
    else if (nanos > THOUSAND) "%.2f %s".format(nanos / THOUSAND.toDouble, micros)
    else nanos + " ns"
  }
  // end copy/paste
  
  def create(impls: (String, () => T)*): Manager = {
    val racers = impls.toList.zipWithIndex map {
      case ((name, fn), id) => Racer(name, id, fn)
    }
    Manager(racers)
  }
  
  case class Manager(racers: List[Racer]) {
    var totalLaps = 0
    val paceCar = racers.head
    
    def racerSpeed(r: Racer) = nanoString(r.nanos / totalLaps)
    def racersString() = 
      if (totalLaps == 0) ""
      else racers map (x => "  " + racerString(x)) mkString "\n"
    def racerString(r: Racer) = r.label + " (" + racerSpeed(r) + " /lap)"
    
    def warmup(): Unit = ()
    def laps(num: Int): List[T] = List.fill(num)(lap())
    def lap(): T = {
      var result: T = null.asInstanceOf[T]
      var toCheck: List[T] = Nil
      shuffle(racers) foreach { racer =>
        val out = racer.run()
        if (racer eq paceCar)
          result = out
        else if (check ne null)
          toCheck ::= out
      }
      totalLaps += 1
      if (check ne null)
        toCheck foreach (x => check(result, x))

      result
    }
    
    override def toString() = {
      """
        | Track: %s
        |Racers: %s
        |  Laps: %s
        |%s""".stripMargin.format(trackName, racers map (_.label) mkString ", ", totalLaps, racersString())
    }

    system addShutdownHook { Console println toString() }
  }
  
  case class Racer(name: String, id: Int, fn: () => T) {
    var totalNanos = 0L

    def label = name + "/#" + id    
    def nanos = totalNanos
    def millis = totalNanos / 1000 / 1000
    
    def run(): T = {
      val t1 = System.nanoTime
      val result = fn()
      val t2 = System.nanoTime
      
      totalNanos += (t2 - t1)
      result
    }
    override def toString = label
  }
}

object Racing {
  def apply[T](impls: (String, () => T)*)(implicit caller: Caller) = {
    val track = new Racing[T](caller.toString, null) { }
    val mgr = track.create(impls: _*)
    mgr.warmup()
    mgr
  }
  
  def demo(max: Int) = apply(
    ("sum" -> (() => 1 to max sum)),
    ("reduce" -> (() => 1 to max reduceLeft (_ + _))),
    ("fold" -> (() => (1 to max).foldLeft(0)(_ + _)))
  )
}
