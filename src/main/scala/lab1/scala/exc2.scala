package lab1.scala

object exc2 {
  def main(args:Array[String]):Unit = {
    val count : Int= 10
    recurse(0)

    def recurse(currentStep : Int): Unit = {
      printHelloX(currentStep)
      if(currentStep < count) {recurse(currentStep+1)}
    }

    def printHelloX(i : Int) :Unit = {
      if(i % 2 == 0) {println("hello " + i)}
      else {println("hello " + count.-(i))}
    }
  }
}
