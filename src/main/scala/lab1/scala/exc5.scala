package lab1.scala

object exc5 {
  def main(args:Array[String]):Unit = {
    println(patterMatch(1))
    println(patterMatch(2))
    println(patterMatch(666))

  }

}

def patterMatch(x : Int) : String = {
  x match{
  case 0 => "zero"
  case 1 => "one"
  case 2 => "two"
  case _ => "other"
  }

}
