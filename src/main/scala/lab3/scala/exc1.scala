package lab3.scala

//#full-example

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import lab3.scala.GreeterMain.SayHello

//#greeter-actor
//Этот актор логирует Hello + {name} и передает сообщение следующему актору
object Greeter {
  //Типы данных для приема и передачи сообщения
  final case class Greet(whom: String, replyTo: ActorRef[Greeted])
  final case class Greeted(whom: String, from: ActorRef[Greet])

  //Вызывается при приеме сообщения
  def apply(): Behavior[Greet] = Behaviors.receive { (context, message) =>

    //Логирование сообщения
    context.log.info("Hello {}!", message.whom)

    //Передача сообщения следующему актору
    message.replyTo ! Greeted(message.whom, context.self)

    //Не меняет поведение
    Behaviors.same
  }
}

//Этот актор логирует Greeting {} for {}
//
object GreeterBot {

  def apply(max: Int): Behavior[Greeter.Greeted] = {
    bot(0, max)
  }

  //Создает поведение для актора
  private def bot(greetingCounter: Int, max: Int): Behavior[Greeter.Greeted] =
    Behaviors.receive { (context, message) =>
      //Счетчик
      val n = greetingCounter + 1
      //Само логирование
      context.log.info("Greeting {} for {}", n, message.whom)
      if (n == max) {
        //Остановка
        Behaviors.stopped
      } else {
        //Передает сообщение и уходит в рекурсию
        message.from ! Greeter.Greet(message.whom, context.self)
        bot(n, max)
      }
    }
}


//Система для работы с акторами
object GreeterMain {
  //Тип данных
  final case class SayHello(name: String)
  //Создает поведение для актора
  def apply(): Behavior[SayHello] =
    //setup поведение при создании объекта
    Behaviors.setup { context =>

      //Создаем потомка актора
      val greeter = context.spawn(Greeter(), "greeter")

      //Определяем поведение
      Behaviors.receiveMessage { message =>
        //Создаем еще актора
        val replyTo = context.spawn(GreeterBot(max = 3), message.name)
        //Передаем сообщение в первого актора, посколькому мы передали ссылку на
        //заранее созданного актора, после логирования сообщение отправляется ему
        //передаем сообщение в рекурсию с глубиной 3, получаем на выходе 3 hello и 3 greeting for Charles
        greeter ! Greeter.Greet(message.name, replyTo)
        Behaviors.same
      }
    }
}


object AkkaQuickstart extends App {


  val greeterMain: ActorSystem[GreeterMain.SayHello] = ActorSystem(GreeterMain(), "AkkaQuickStart")

  //Отсылаем первое сообщение
  greeterMain ! SayHello("Charles")

}