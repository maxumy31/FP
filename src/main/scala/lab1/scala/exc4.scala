package lab1.scala

object exc4 {

  def main(args:Array[String]):Unit = {
    var a : (Int) => (Int) = x => x * 2
    println(a)
    //Судя по всему, вывелся адрес функции
  }

}
