import concurrent.jolib._;
import concurrent.SyncVar;

/** Implementation in the join-calculus of a parallel OR. */
object or extends Join {
  
  object res extends Synchr[boolean](this) { case class C() extends SyncVar[boolean] };
  object res1 extends Asynchr(this) { case class C(b: boolean); }
  object res2 extends Asynchr(this) { case class C(b: boolean); }
  object res1False extends Synchr[boolean](this) { case class C() extends SyncVar[boolean] };
  object res2False extends Synchr[boolean](this) { case class C() extends SyncVar[boolean] };
  
  rules(
    Pair(List(res, res1), { case List(r @ res.C(), res1.C(b)) =>
      if (b) r.set(b) else r.set(res1False.send(res1False.C())) }),

    Pair(List(res, res2), { case List(r @ res.C(), res2.C(b)) =>
      if (b) r.set(b) else r.set(res2False.send(res2False.C())) }),

    Pair(List(res1False, res2),  { case List(r @ res1False.C(), res2.C(b)) =>
      r.set(b) }),

    Pair(List(res2False, res1),  { case List(r @ res2False.C(), res1.C(b)) =>
      r.set(b) })
  );
  
  def apply(def b1: boolean, def b2: boolean): boolean = {
    concurrent.ops.spawn(res1.send(res1.C(b1)));
    concurrent.ops.spawn(res2.send(res2.C(b2)));
    res.send(res.C())
  }
}

object parallelOr {

  def main(args: Array[String]): unit = {
    def loop: boolean = { while (true) {}; true };
    System.out.println("true || true = " + or(true, true));
    System.out.println("false || false = " + or(false, false));
    System.out.println("false || true = " + or(false, true));
    System.out.println("true || false = " + or(true, false));
    System.out.println("true || loop = " + or(true, loop));
    System.out.println("loop || true = " + or(loop, true));
  }

}
