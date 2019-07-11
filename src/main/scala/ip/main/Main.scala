package ip.main

import akka.actor.ActorSystem
import ip.actor.Coordinator

import scala.concurrent.Await
import scala.io.StdIn
import scala.concurrent.duration._
object Main {

  def main(args: Array[String]): Unit = {

    val actorSystem = ActorSystem()


    val actor = actorSystem.actorOf(Coordinator.props)

    val startUrl = "https://www.imobiliare.ro/vanzare-apartamente/cluj-napoca?id=141495822"
    actor ! Coordinator.Start(startUrl, maxPages = 50)

    println("processing, press any key to exit....")
    StdIn.readLine()

    Await.ready(actorSystem.terminate(), 5 seconds)
    println("Goodbye!")

  }
}
