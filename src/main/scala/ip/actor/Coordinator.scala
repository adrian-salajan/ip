package ip.actor

import java.net.URL
import java.util.concurrent.TimeUnit

import akka.actor.{Actor, Props}
import ip.actor.Coordinator.{ProcessPage, SimpleViewResults, Start}
import ip.service._
import akka.pattern.pipe
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import ip.actor.SimpleViewActor.GetSimpleViews

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.util.Random

class Coordinator(
  parser: Parser,
  csvService: CsvService
) extends Actor {

  override def receive: Receive = waitForStart()

  implicit val ec: ExecutionContext = context.dispatcher
  implicit val timeout: Timeout = 5.seconds

  def waitForStart(): Receive = {
    case Start(url, maxPages) =>
      val firstPage = parser.pageContents(new URL(url))
      val pageLinks = firstPage.flatMap(parser.pageUrls)
      val toDo = pageLinks.fold(t => {
        println(t)
        List.empty
      }, pl => pl)

      context become processing(toDo.take(maxPages), List.empty)
      self ! ProcessPage


      def processing(toDo: List[PageUrlNumbered], finished: List[SimpleView]): Receive = {
        case ProcessPage =>
          if (toDo.isEmpty) {
            writeResultToFile(finished)
          } else {
            val next = toDo.headOption
            next.foreach(parseNextPage)
          }

        case SimpleViewResults(results) =>
          context become processing(toDo.drop(1), finished ++ results)
          val randomDuration = FiniteDuration(Random.nextInt(3) + 1, TimeUnit.SECONDS)
          context.system.scheduler.scheduleOnce(randomDuration, self, ProcessPage)
      }
  }

  private def parseNextPage(pageUrl: PageUrlNumbered) = {
    val actor = context.actorOf(SimpleViewActor())
    val simpleViews = actor ? GetSimpleViews(pageUrl)
    simpleViews.pipeTo(self)
  }

  private def writeResultToFile(finished: List[SimpleView]) = {
    println("all Finished")
    val finishedCsv = csvService.exportToCsv(finished)
    csvService.writeToFile(finishedCsv, "simple_views.csv")
  }
}

object Coordinator {
  def props(): Props = Props(new Coordinator(WebParser(), new CsvService()))


  case class Start(sessionUrl: String, maxPages: Int)
  case object ProcessPage
  case class  SimpleViewResults(simpleViews: List[SimpleView])
}
