package ip.actor

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, PoisonPill, Props}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import ip.actor.Coordinator.{ProcessPage, SimpleViewResults, Start}
import ip.actor.SimpleViewActor.GetSimpleViews
import ip.service._
import org.apache.commons.lang3.exception.ExceptionUtils

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Random

class Coordinator(
  parser: Parser,
  csvService: CsvService
) extends Actor {

  override def receive: Receive = waitForStart()

  implicit val ec: ExecutionContext = context.dispatcher
  implicit val timeout: Timeout = 5.seconds
  var currentPage = 0

  def waitForStart(): Receive = {
    case Start(url, maxPages) =>
      val firstPage = parser.pageContents(url)
      val pageLinks = firstPage.flatMap(parser.pageUrls)

      pageLinks.fold(t => {
        println(url)
        ExceptionUtils.printRootCauseStackTrace(t)
        self ! PoisonPill
      }, toDo => {
        context become processing(toDo.take(maxPages - 1), List.empty)
        self ! ProcessPage
      })



      def processing(toDo: List[PageUrlNumbered], finished: List[SimpleView]): Receive = {
        case ProcessPage =>
          currentPage = currentPage + 1
          println("page = " + currentPage)
          if (toDo.isEmpty) {
            writeResultToFile(finished)
          } else {
            val next = toDo.headOption
            next.foreach(parseNextPage)
          }

        case SimpleViewResults(results) =>
          context become processing(toDo.drop(1), finished ++ results)

          context.system.scheduler.scheduleOnce(Coordinator.RandomDurationBetweenPages, self, ProcessPage)
      }
  }

  private def parseNextPage(pageUrl: PageUrlNumbered) = {
    val actor = context.actorOf(SimpleViewActor.props(parser))
    val simpleViews = actor ? GetSimpleViews(pageUrl)
    simpleViews.pipeTo(self)
  }

  private def writeResultToFile(finished: List[SimpleView]) = {
    println("all Finished, total = " + finished.size)
    val finishedCsv = csvService.exportToCsv(finished)
    csvService.writeToFile(finishedCsv, "simple_views.csv")
  }
}

object Coordinator {
  def props: Props = Props(new Coordinator(new WebParser(), new CsvService()))

  val RandomDurationBetweenPages = FiniteDuration(Random.nextInt(3000) + 1000, TimeUnit.MILLISECONDS)
  case class Start(sessionUrl: String, maxPages: Int)
  case object ProcessPage
  case class  SimpleViewResults(simpleViews: List[SimpleView])
}
