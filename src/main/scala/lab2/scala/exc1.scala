package lab2.scala

object exc1 {
  def main(args:Array[String]):Unit = {
    println(integralRec(x=>x,0,10,1000))
    println(integralMapReduce(x=>x,0,10,1000))
  }
}

def integralRec(f: Double =>Double, l : Double, r : Double, steps : Int) : Double = {
  def calculateStep(l: Double , size:Double,leftMore:Int):Double = {
    if(leftMore == 0) 0 else f(l) * size+ calculateStep(l+size,size,leftMore-1)
  }
  calculateStep(l,((r-l)/steps),steps)
}

def integralMapReduce(f: Double =>Double, l : Double, r : Double, steps : Int) : Double = {
  val stepSize = ((r-l)/steps)
  val iSeq = (0 until steps).map(x => f( l + x * stepSize) * stepSize)
  iSeq.sum
}
