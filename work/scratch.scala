trait MemoVerify[K, V] extends MemoUtil {
  self: Memo[K, V] =>

  addMessage(
    Seq(
      "Frequently seen call sites (depth %d)\n".format(originsDepth),
      "%d sites have %d distinct traces.\n".format(callSites.size, origins.size),
      topCallers()
    )
  )

  def onDifference(key: K, v1: V, v2: V): Unit = {
    report("%s issue with key %s:\n  map: %s '%s'\n    f: %s '%s'".format(
      label, key,
      v1.asInstanceOf[AnyRef].getClass.getName, v1,
      v2.asInstanceOf[AnyRef].getClass.getName, v2)
    )
  }
  def compareValues(v1: V, v2: V): Boolean = v1 == v2
  
  def verify(key: K): V = {
    val isHit = this contains key
    val v1    = this(key)
    
    if (!isHit)
      return v1
    
    val v2 = f(key)
    if (!compareValues(v1, v2)) {
      onDifference(key, v1, v2)
      if (v1.toString == v2.toString)
        report("  ...but the Strings are equal!")

    }
    v2
  }
}
