package lab1.scala

object exc6 {
  def main(args:Array[String]):Unit = {
    val times2: Int =>Int = x => x * 2
    println(compose(times2,times2)(2))
  }
}

def compose[A, B, C](f: B => C, g: A => B): A => C = {
  x => f(g(x))
}
