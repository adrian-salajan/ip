package ip.actor

import akka.actor.{Actor, PoisonPill, Props}
import ip.actor.SimpleViewActor.GetSimpleViews
import ip.service.{PageUrlNumbered, Parser}

class SimpleViewActor(parser: Parser) extends Actor {
  override def receive: Receive = waitingToStart()


  def waitingToStart(): Receive = {
    case GetSimpleViews(pageUrl) =>
      val simpleViews = for {
        pc <- parser.pageContents(pageUrl.url)
        result <- parser.simpleViews(pc)
      } yield result

      simpleViews.fold(t => {
        println(t)
        sender() ! Coordinator.SimpleViewResults(List.empty)
      },
        views => sender() ! Coordinator.SimpleViewResults(views)
      )

      self ! PoisonPill
  }
}

object SimpleViewActor {

  def props(parser: Parser):Props = Props(new SimpleViewActor(parser))

  case class GetSimpleViews(pageUrl: PageUrlNumbered)
}
