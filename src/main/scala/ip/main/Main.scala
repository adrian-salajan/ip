package ip.main

import akka.actor.ActorSystem
import ip.actor.Coordinator

object Main {

  def main(args: Array[String]): Unit = {

    val actorSystem = ActorSystem()


    val actor = actorSystem.actorOf(Coordinator.props)

    val startUrl = "https://www.imobiliare.ro/vanzare-apartamente/cluj-napoca?id=6981616"
    actor ! Coordinator.Start(startUrl, maxPages = 2)

  }
}
