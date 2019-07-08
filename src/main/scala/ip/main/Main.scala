package ip.main

import akka.actor.ActorSystem
import ip.actor.Coordinator

class Main {
  def main(args: Array[String]): Unit = {

    val actorSystem = ActorSystem()


    val actor = actorSystem.actorOf(Coordinator.props)

    val startUrl = ""
    actor ! Coordinator.Start(startUrl, maxPages = 3)


  }
}
