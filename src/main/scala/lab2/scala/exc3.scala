package lab2.scala

import scala.compiletime.uninitialized


object exc3 {
  def main(args: Array[String]): Unit = {
    val seq: Seq[Int] = List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    var functor = new FunctorClass(seq)
    println(functor)

    var mappedFunctor = functor.map(x => x * 10)
    println(mappedFunctor)

    var tryMonad1 : Monad[Int] = new TryMonad[Int](() => 5 / 0)
    println(tryMonad1)


    val operation1 = new TryMonad(() => 5)
    val result = operation1
      .flatMap(x => new TryMonad(() => x + 14))
      .flatMap(x => new TryMonad(() => x * 228))
      .flatMap(x => new TryMonad(() => if (x > 420) throw new Exception("Too large!") else x))
    println(result)
  }
}



trait Functor[A] {
  def map(f: A => A): Functor[A]
}

class FunctorClass[A](data: Iterable[A]) extends Functor[A] {
  override def map(f: A => A): Functor[A] = {
    var newSeq: Seq[A] = Seq()
    for (v <- data) {
      newSeq = newSeq.appended(f(v))
    }
    new FunctorClass(newSeq)
  }

  override def toString: String = data.toString
}

trait Monad[A] {
  def unit(a : A) : Monad[A]
  def flatMap[B](f: A => Monad[B]) : Monad[B]
}

class TryMonad[A](f : () => A) extends Monad[A]{


  var exception : Exception = uninitialized
  var result : A = uninitialized
  var isSuccess : Boolean = false

  execute()


  override def unit(a: A): Monad[A] = {
    new TryMonad(() => a)
  }

  private def execute() : Unit = {
    try {
      var r = f()
      result = r
      isSuccess = true
    }
    catch {
      case t: Throwable =>
        exception = new Exception(t)
        isSuccess = false
    }
  }

  override def flatMap[B](f: A => Monad[B]): Monad[B] = {
    if(isSuccess) {
      f(result)
    } else {
      var uninit: B = null.asInstanceOf[B]
        new TryMonad(() =>
          uninit
        )
    }
  }

  override def toString: String = {
    if (isSuccess) "Success : " + result.toString else "Failure : " + exception.toString
  }
}