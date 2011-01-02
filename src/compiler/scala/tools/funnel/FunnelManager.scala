/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package funnel

class FunnelManager {
  private var active: List[Memo[_, _]] = Nil
  private def isDump = Config.isSet("dump")

  def processActive() = {
    active foreach { funnel =>
      if (isDump)
        funnel.dump()
      
      funnel.report(funnel.finalSummary())
    }
    active = Nil
  }
  private[funnel] def reportAtShutdown[T <: Memo[_, _]](memo: T): T = {
    /** Create the shutdown hook if/when first one is added. */
    if (active.isEmpty)
      system addShutdownHook processActive()

    if (active contains memo) ()
    else active :+= memo
    
    memo
  }
}

object FunnelManager extends FunnelManager {
  
}