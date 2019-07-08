package ip.service

import java.net.URL

import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import scala.util.Try
import cats.syntax.either._
import cats.syntax.traverse._
import cats.instances.list._
import cats.instances.either._


sealed trait Page
case class PageUrl(url: URL) extends Page
case class PageUrlNumbered(url: URL, pageNumber: Int) extends Page
case class PageContents(contents: String) extends Page


trait Parser {

  def pages(page: PageContents): Either[Throwable, List[PageUrlNumbered]]

  def simpleView(page: PageContents): Either[Throwable, List[SimpleView]]

}


class WebParser extends Parser {

  import scala.collection.JavaConverters._

  override def pages(page: PageContents): Either[Throwable, List[PageUrlNumbered]] = {
    Try {

      val buttons = Jsoup.parse(page.contents).select(".index_paginare a.butonpaginare:not(.inainte)")
        .iterator().asScala.toList

      val lastButton = buttons.last
      val lastPage = PageUrlNumbered(new URL(lastButton.absUrl("href")), Integer.parseInt(lastButton.text()))

      for {
        pn <- 1 to lastPage.pageNumber
        stringUrl = lastPage.url.toString
        baseUrl = stringUrl.substring(0, stringUrl.lastIndexOf("=") + 1)
      } yield PageUrlNumbered(new URL(baseUrl + pn), pn)

    }.map(_.toList).toEither

  }


  private def parseFloor(floor: String): (Int, Int) = {
    val parts = floor.split("/")
    val maxFloor = Integer.parseInt(parts(1))
    if (floor.toLowerCase.contains("parter")) (0, maxFloor)
    else (parts(0).filter(d => d.isDigit).toInt, maxFloor)

  }

  private def parseCompariment(compartiment: String): Compartiment = compartiment.toLowerCase match {
    case "decomandat" => Decomandat
    case "nedecomandat" => Nedecomandat
    case "circular" => Circular
    case other => Other(other)
  }

  private def parseParts(details: Element): Parts = {
    val parts  = WebParser.SimpleViewPartsOrder.zip(details.select("li").asScala.toList.map(_.text()))
    val partsToText = parts.toMap

    val (floor, maxFloor) = parseFloor(partsToText(FloorPart))
    Parts(
      Integer.parseInt(partsToText(Rooms).filter(_.isDigit)),
      partsToText(Surface).filter(d => d.isDigit || '.'.equals(d)).toDouble.intValue(),
      floor,
      maxFloor,
      parseCompariment(partsToText(CompartimentPart)),
      partsToText.contains(IsNew)
    )

  }

  case class Parts(rooms: Int, surface: Int, floor: Int, maxFloor:Int, c: Compartiment, isNew: Boolean)

  private def parseSimpleView(id: String, url: URL, details: Element): Either[Throwable, SimpleView] = Try {
    val parts = parseParts(details)
    SimpleView(id, url, parts.surface, parts.rooms, Floor(parts.floor, parts.maxFloor), parts.c,
      if (parts.isNew) NewBuilding else Year(0) //TODO
    )
  }.toEither

  override def simpleView(page: PageContents): Either[Throwable, List[SimpleView]] = {
    val boxes = Try {
      val doc = Jsoup.parse(page.contents)
      doc.select(".box-anunt:not(.anunt-special)").iterator().asScala.toList
    }.toEither

    boxes.flatMap { boxes =>
      val all: List[Either[Throwable, SimpleView]] = for {
        box <- boxes
        idParts = box.id().split("-")
        id = if (idParts.size > 1) idParts(1) else "noId"
        url = new URL(box.selectFirst(".row .mobile-container-url").absUrl("href"))
        descriptionElement = box.selectFirst(".caracteristici")
        simpleView = parseSimpleView(id, url, descriptionElement)
      } yield simpleView

      all.sequence.asInstanceOf[Either[Throwable, List[SimpleView]]] //TODO partialunification
    }


  }
}


sealed trait SimpleViewParts extends Product with Serializable
case object Rooms extends SimpleViewParts
case object Surface extends SimpleViewParts
case object FloorPart extends SimpleViewParts
case object CompartimentPart extends SimpleViewParts
case object IsNew extends SimpleViewParts

object WebParser {
  val SimpleViewPartsOrder = List(Rooms, Surface, FloorPart, CompartimentPart, IsNew)

}
