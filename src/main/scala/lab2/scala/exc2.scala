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


    testWithWord("test",goodEnoughPasswordWithEither)
    testWithWord("Test",goodEnoughPasswordWithEither)
    testWithWord("Test1",goodEnoughPasswordWithEither)
    testWithWord("Test1~",goodEnoughPasswordWithEither)
    testWithWord("Test1~AAAAAA",goodEnoughPasswordWithEither)

    readPasswordAndPrintResult()
  }
}



def testWithWord[A](w:String, func : String => A):A = {
  var res : A = func(w)
  println("Testing word " + w + " with function " + func + ". Result is " + res)
  res
}

def goodEnoughPasswordWithEither(password:String):Either[String,String] = {
  def AtLeastOneChar(s:String,mustContainCharString:String,error:String) :Either[String,String] = {
    if (mustContainCharString.exists( m =>
      s.contains(m)
    )) Right(s) else Left(error)
  }

  def check(s : Seq[() => Either[String,String]]) : Either[String,String] = {
    if (s.isEmpty) return Right(password)

    s.head() match {
      case Left(error) => Left(error)
      case Right(_) => check(s.drop(1))
    }
  }

  var chain = Seq[() => Either[String,String]]()
  chain = chain.appended(() => AtLeastOneChar(password,"ABCDEFGHIJKLMNOPQRSTUVWXYZ","Need uppercase character"))
  chain = chain.appended(() => AtLeastOneChar(password,"abcdefghijklmnopqrstuvwxyz","Need lowercase character"))
  chain = chain.appended(() => AtLeastOneChar(password,"1234567890","Need digit"))
  chain = chain.appended(() => AtLeastOneChar(password,"!@#$%^&*()_+=`~;./][{}/\\?|\"","Need special character"))
  chain = chain.appended(() => if (password.length >= 8) Right(password) else Left("Password must contain at least 8 characters"))

  check(chain)
}


def readPasswordAndPrintResult() = {
  def decipherResult(t : Try[String]) : String = {
    t match {
      case Success(t) => t
      case Failure(exception) => ""
    }
  }

  val result = decipherResult(Await.ready(readPassword(),Duration.Inf).value.get)
  println(result)
}

def readPassword():Future[String] = {
  var password : String = ""

  def resultToString(e : Either[String,String]) : String = {
    e match {
      case Left(x) => x
      case Right(x) => password
    }}

  println("Enter your password")
  password = StdIn.readLine()

  var f : Future[String] = Future(resultToString(goodEnoughPasswordWithEither(password)))(global)

  f.onComplete((f) => f)(global)

  f
}

//Я не понимаю, как тут использовать Option....
def goodEnoughPassword(password:String):Boolean = {
  def atLeastOneChar(s:String,mustContainCharString:String) :Boolean = {
    if (mustContainCharString.exists( m =>
      s.contains(m)
    )) true else false
  }

  Some(password).filter((s) => atLeastOneChar(s,"ABCDEFGHIJKLMNOPQRSTUVWXYZ")).
    filter((s) => atLeastOneChar(s,"abcdefghijklmnopqrstuvwxyz")).
    filter((s) => atLeastOneChar(s,"1234567890")).
    filter((s) => atLeastOneChar(s,"!@#$%^&*()_+=`~;./][{}/\\?|\"")).
    filter((s) => s.length > 8) match {
    case Some(x) => true
    case _ => false
  }

}

