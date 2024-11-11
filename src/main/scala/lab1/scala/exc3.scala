package lab1.scala


object exc3 {

  def main(args:Array[String]):Unit = {
    val seq = Seq[Int](1,2,3,4,5,6,7,8,10)

    println(seq)
    println(separateMap(seq))
    println(separateFilter(seq))
    println(max(seq))


  }

}
def separateMap(s : Seq[Int]) : (Seq[Int],Seq[Int]) = {
  var a = Seq[Int]()
  var b = Seq[Int]()


  def passValueForIndex(a : Seq[Int],b:Seq[Int],v : Int, i : Int) : (Seq[Int],Seq[Int]) = {
    if (i % 2 == 0) (a.appended(v), b) else (a, b.appended(v))
  }

  var id = 0
  s.map(v => {
    val (na,nb) = passValueForIndex(a,b,v,id)
    a = na
    b = nb
    id = id + 1
  })

  return (a,b)
}

def separateFilter(s : Seq[Int]) : (Seq[Int],Seq[Int]) = {
  var a = Seq[Int]()
  var b = Seq[Int]()

  var id = -1
  a = s.filter(_ => {
    id = id + 1
    (id % 2 == 0)
  } )
  id = -1
  b = s.filter(_ => {
    id = id + 1
    (id % 2 != 0)
  } )

  (a,b)
}

def max(s :Seq[Int]) : Int = {
  s.reduce((a,b) => if(a > b) a else b)
}

