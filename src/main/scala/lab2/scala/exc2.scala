package lab2.scala

import scala.concurrent.ExecutionContext.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object exc2 {
  def main(args:Array[String]):Unit = {


    testWithWord("test",goodEnoughPassword)
    testWithWord("Test",goodEnoughPassword)
    testWithWord("Test1",goodEnoughPassword)
    testWithWord("Test1~",goodEnoughPassword)
    testWithWord("Test1~AAAAAA",goodEnoughPassword)

    println("#####################################################")
    println("#####################################################")
    println("#####################################################")


    testWithWord("test",goodEnoughPasswordEith)
    testWithWord("Test",goodEnoughPasswordEith)
    testWithWord("Test1",goodEnoughPasswordEith)
    testWithWord("Test1~",goodEnoughPasswordEith)
    testWithWord("Test1~AAAAAA",goodEnoughPasswordEith)

    readPasswordAndPrintResult()
  }
}



def testWithWord[A](w:String, func : String => A):A = {
  var res : A = func(w)
  println("Testing word " + w + " with function " + func + ". Result is " + res)
  res
}

def goodEnoughPasswordEith(password:String):Either[String,Boolean] = {
  def AtLeastOneChar(s:String,mustContainCharString:String,error:String) :Either[String,Boolean] = {
    if (mustContainCharString.exists( m =>
      s.contains(m)
    )) Right(true) else Left(error)
  }

  def check(s : Seq[() => Either[String,Boolean]]) : Either[String,Boolean] = {
    if (s.isEmpty) return Right(true)

    s.head() match {
      case Left(error) => Left(error)
      case Right(_) => check(s.drop(1))
    }
  }

  var chain = Seq[() => Either[String,Boolean]]()
  chain = chain.appended(() => AtLeastOneChar(password,"ABCDEFGHIJKLMNOPQRSTUVWXYZ","Need uppercase character"))
  chain = chain.appended(() => AtLeastOneChar(password,"abcdefghijklmnopqrstuvwxyz","Need lowercase character"))
  chain = chain.appended(() => AtLeastOneChar(password,"1234567890","Need digit"))
  chain = chain.appended(() => AtLeastOneChar(password,"!@#$%^&*()_+=`~;./][{}/\\?|\"","Need special character"))
  chain = chain.appended(() => if (password.length >= 8) Right(true) else Left("Password must contain at least 8 characters"))

  check(chain)
}


def readPasswordAndPrintResult() = {
  def decipherResult(t : Try[String]) : String = {
    t match {
      case Success(t) => t
      case Failure(exception) => ""
    }
  }

  val f = readPassword()
  val resultTry = Await.ready(f,Duration.Inf).value.get
  val resultString = decipherResult(resultTry)
  println(resultString)
}

def readPassword():Future[String] = {
  var password : String = ""

  def resultToString(e : Either[String,Boolean]) : String = {
    e match {
      case Left(x) => x
      case Right(x) => password
    }}

  println("Enter your password")
  password = StdIn.readLine()

  var f : Future[String] = Future(resultToString(goodEnoughPasswordEith(password)))(global)

  while(!f.isCompleted) {
    Thread.sleep(10)
  }

  f
}

//Я не понимаю, как тут использовать Option....
def goodEnoughPassword(password:String):Boolean = {
  def atLeastOneChar(s:String,mustContainCharString:String) :Boolean = {
    if (mustContainCharString.exists( m =>
      s.contains(m)
    )) true else false
  }

  atLeastOneChar(password,"ABCDEFGHIJKLMNOPQRSTUVWXYZ") &&
    atLeastOneChar(password,"abcdefghijklmnopqrstuvwxyz") &&
    atLeastOneChar(password,"1234567890") &&
    atLeastOneChar(password,"!@#$%^&*()_+=`~;./][{}/\\?|\"") &&
    password.length > 8
}

