package lab3.scala

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout
import lab3.scala.AddingServer.AddMessage

import java.lang.Thread.sleep
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.language.postfixOps
import scala.util.{Random, Try}


object AddingServer:

  case class AddMessage(a:Int, b:Int, from : ActorRef[Int])

  def apply[A]():Behavior[AddMessage] = Behaviors.receive{
    (context, message)=>

        context.log.info("server took two number: " + message.a.toString + " and " + message.b.toString)
        val sum = message.a + message.b
        context.log.info("sum of " + message.a.toString + " and " + message.b.toString + " is " + sum.toString)
        message.from ! sum
        Behaviors.same
      }


object AddingClient:
  def apply(server : ActorRef[AddMessage]):Behavior[Int] = Behaviors.receive{
    (context,message) =>
      val random = new Random()
      random.setSeed(message)
      given timeout:Timeout = Timeout(Duration.create(5, TimeUnit.SECONDS))

      val randomValue1 = random.nextInt(1000000)
      val randomValue2 = random.nextInt(1000000)

      context.log.info("generated random numbers are " + randomValue1.toString + " and " + randomValue2.toString)

      val add = AddMessage(randomValue1,randomValue2,context.self)

      context.ask[AddMessage, Int](
        server,
        ref => AddMessage(randomValue1, randomValue2, ref)
      )( {
        case scala.util.Success(res) =>
          context.log.info("number recieved : " + res)
          res
        case scala.util.Failure(exception) =>
          context.log.error(s"failed to receive result: ${exception.getMessage}")
          -1
      })(timeout)

      Behaviors.same



  }

//Наш ActorSystem
object AddingSystem:
  //Поведение при инициализации ActorSystem
  def apply():Behavior[Unit] = Behaviors.setup{ (context)=>
    val adder = context.spawn(AddingServer(), "adder")
    val addingClient = context.spawn(AddingClient(adder),"client")

    addingClient ! 123



    Behaviors.receive{ (context, message) =>
      context.log.info(message.toString())
      Behaviors.stopped
    }
  }
@main def AddingMain():Unit =
  val system = ActorSystem(AddingSystem(),"system")
