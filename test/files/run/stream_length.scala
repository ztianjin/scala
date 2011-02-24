

object Test {
  def walk(depth: Int, bias: String): Stream[String] = {
    if (depth == 0)
      Stream(bias)
    else {
      Stream.range(1, 100).map((x: Int) => walk(depth-1, bias + x)).flatten
    }
  }

  def main(args: Array[String]) {
    println("Length: " + walk(3, "---").length)
  }
}
