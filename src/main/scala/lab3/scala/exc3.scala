package lab3.scala

import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout
import lab3.scala.IntegralCalculator.ResultSummator
import lab3.scala.IntegralServer.{IntegralDescription, IntegralDescriptionWithReply}

import java.lang.Thread.sleep
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.language.postfixOps
import scala.util.{Random, Try}


object IntegralServer:

  case class IntegralDescription(l:Double, r:Double, f: Double => Double, s: Int)
  case class IntegralDescriptionWithReply(l:Double, r:Double, f: Double => Double, s: Int, replyTo : ActorRef[Double])

  def apply():Behavior[IntegralDescription] = Behaviors.setup{ (context) =>
    val splitToParts:Int = 10

    Behaviors.receive{
    (context, message)=>
      val stepsSize = (message.r - message.l) / message.s
      val partsCount = message.s / splitToParts
      val remainingSteps = message.s % splitToParts

      val sumActor = context.spawn(ResultSummator(splitToParts,null),"sum")

      val parts : Seq[IntegralDescriptionWithReply] = Range.inclusive(0,splitToParts-1).map((x) =>
        {
          val start = message.l + x * partsCount * stepsSize
          val end = if(x == splitToParts-1) message.r else start + partsCount * stepsSize
          val steps = if(x==splitToParts-1) partsCount + remainingSteps else partsCount
          IntegralDescriptionWithReply(start,end,message.f,steps,sumActor)
        })

      val calculators: Seq[ActorRef[IntegralDescriptionWithReply]] = parts.zipWithIndex.map{case(x,i) => context.spawn(IntegralCalculator(),"calculator" + i)}

      calculators.zip(parts).foreach{ case (calculator, part) =>
        calculator ! part
      }

      context.log.info("server took integral description " + message.toString)
      Behaviors.same
  }}


object IntegralCalculator:
  def apply():Behavior[IntegralDescriptionWithReply] = Behaviors.receive{
    (context,message) =>

      def calculateStep(l: Double, size: Double, leftMore: Int,f: Double=>Double): Double = {
        if (leftMore == 0) 0 else f(l) * size + calculateStep(l + size, size, leftMore - 1,f)
      }
      val res = calculateStep(message.l,((message.r-message.l)/message.s),message.s,message.f)
      message.replyTo ! res
      Behaviors.stopped

  }

  object ResultSummator:

    def apply(totalParts: Int, replyTo: ActorRef[Double]): Behavior[Double] = Behaviors.setup { context =>
      var collectedResults: Seq[Double] = Seq.empty

      Behaviors.receive { (context, message) =>
          collectedResults = collectedResults.appended(message)
          context.log.info("Received partial result: " + message)
          if (collectedResults.size == totalParts)
          {
            val finalResult = collectedResults.sum
            context.log.info("Final result calculated: " + finalResult)
            Behaviors.stopped
          } else {
            Behaviors.same
          }
      }
    }

object IntegralSystem:
  def apply():Behavior[Unit] = Behaviors.setup{ (context)=>
    val integralServer = context.spawn(IntegralServer(), "integralServer")

    integralServer ! IntegralDescription(0,10,(_ * 1),100)


    Behaviors.receive{ (context, message) =>
      context.log.info(message.toString())
      Behaviors.stopped
    }
  }
@main def IntegralMain():Unit =
  val system = ActorSystem(IntegralSystem(),"system")
