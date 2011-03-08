class A
class B extends A
class C extends B

object Test {
  type CCInt = {
    def whee(x: C, y: C): Int
  }
  
  class C0 { def whee(x: C, y: C): Int = 5 }
  class C1[T] { def whee(x: T, y: T): Int = 5 }
  class C2[T <: A] { def whee(x: T, y: T): Int = 5 }
  class C3[T <: B] { def whee(x: T, y: T): Int = 5 }
  class C4[T <: C] { def whee(x: T, y: T): Int = 5 }
  class C5[T <: A] { def whee(x: T, y: C): Int = 5 }
  class C6[T <: B] { def whee(x: T, y: C): Int = 5 }
  class C7[T <: C] { def whee(x: T, y: C): Int = 5 }
  
  class CFail { def whee(x: C, y: C): Int = sys.error("fail") }
  
  def g(x: CCInt) = x.whee(new C, new C)
  def g2(x: CCInt) = {
    val ms = x.getClass.getMethods.toList filter (_.getName == "whee")
    ms.head.invoke(x, new C: AnyRef, new C: AnyRef)
  }
  
  def f(msg: String)(x: CCInt) = {
    print(msg+ ": ")
    try println(g(x))
    catch { case t: NoSuchMethodException =>
      print("no such method, reflecting... ")
      try println(g2(x))
      catch { case t => println(t) }
    }
  }
  
  def main(args: Array[String]): Unit = {
    f("C0")(new C0)
    f("C1")(new C1)
    f("C2")(new C2)
    f("C3")(new C3)
    f("C4")(new C4)
    f("C5")(new C5)
    f("C6")(new C6)
    f("C7")(new C7)
    f("CFail")(new CFail)
  }
}

// Before:
//
// C0: 5
// C1: java.lang.NoSuchMethodException: Test$C1.whee(C, C)
// C2: java.lang.NoSuchMethodException: Test$C2.whee(C, C)
// C3: java.lang.NoSuchMethodException: Test$C3.whee(C, C)
// C4: 5
// C5: java.lang.NoSuchMethodException: Test$C5.whee(C, C)
// C6: java.lang.NoSuchMethodException: Test$C6.whee(C, C)
// C7: 5
//
// After:
//
// C0: 5
// C1: 5
// C2: 5
// C3: 5
// C4: 5
// C5: 5
// C6: 5
// C7: 5
