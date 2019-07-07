package ip.actor

import akka.actor.{Actor, Props}

class Coordinator(startingSessionUrl: String) extends Actor {
  override def receive: Receive = ???
}

object Coordinator {
  def props(startingSessionUrl: String): Props = Props(new Coordinator(startingSessionUrl))
}
