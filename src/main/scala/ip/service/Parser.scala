package ip.service

import java.net.URL

import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import scala.util.Try


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


  def parseFloor(floor: String): (Int, Int) = {
    val parts = floor.split("/")
    val maxFloor = Integer.parseInt(parts(1))
    if (floor.toLowerCase.contains("parter"))
      (0, maxFloor)
    else
      (Integer.parseInt(parts(0).filter(_.isDigit)), maxFloor)
  }

  def parseCompariment(compartiment: String): Compartiment = compartiment.toLowerCase match {
    case "decomandat" => Decomandat
    case "nedecomandat" => Nedecomandat
    case "circular" => Circular
    case other => Other(other)
  }

  def parseParts(details: Element): Parts = {
    val parts  = WebParser.SimpleViewPartsOrder.zip(details.select("li").asScala.toList.map(_.text()))
    val partsToText = parts.toMap

    val (floor, maxFloor) = parseFloor(partsToText(Floor))
    Parts(
      Integer.parseInt(partsToText(Rooms).filter(_.isDigit)),
      Integer.parseInt(partsToText(Surface).filter(_.isDigit)),
      floor,
      maxFloor,
      parseCompariment(partsToText(Compartiment)),
      partsToText.contains(IsNew)
    )

  }

  case class Parts(rooms: Int, surface: Int, floor: Int, maxFloor:Int, c: Compartiment, isNew: Boolean)

  def parseSimpleView(id: String, url: URL, details: Element): SimpleView = {
    val parts = parseParts(details)
    SimpleView(id, url, parts.surface, parts.rooms, Etaj(parts.floor, parts.maxFloor), parts.c,
      if (parts.isNew) BlocNou else An(0) //TODO
    )
  }

  override def simpleView(page: PageContents): Either[Throwable, List[SimpleView]] = {
    Try {
      val doc = Jsoup.parse(page.contents)
      val boxes = doc.select(".box-anunt:not(.anunt-special)").iterator().asScala.toList

      for {
        box <- boxes
        idParts = box.id().split("-")
        id = if (idParts.size > 1) idParts(1) else "noId"
        url = new URL(box.selectFirst(".row .mobile-container-url").absUrl("href"))
        descriptionElement = box.selectFirst(".caracteristici")
        simpleView = parseSimpleView(id, url, descriptionElement)
      } yield simpleView

    }.toEither

  }
}


sealed trait SimpleViewParts extends Product with Serializable
case object Rooms extends SimpleViewParts
case object Surface extends SimpleViewParts
case object Floor extends SimpleViewParts
case object Compartiment extends SimpleViewParts
case object IsNew extends SimpleViewParts

object WebParser {
  val SimpleViewPartsOrder = List(Rooms, Surface, Floor, Compartiment, IsNew)

}
