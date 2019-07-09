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
case class PageUrlNumbered(url: String, pageNumber: Int) extends Page
case class PageContents(contents: String) extends Page


trait Parser {

  def pageContents(uRL: String): Either[Throwable, PageContents]

  def pageUrls(page: PageContents): Either[Throwable, List[PageUrlNumbered]]

  def simpleViews(page: PageContents): Either[Throwable, List[SimpleView]]

}


class WebParser extends Parser {

  import scala.collection.JavaConverters._

  override def pageUrls(page: PageContents): Either[Throwable, List[PageUrlNumbered]] = {
    Try {

      val buttons = Jsoup.parse(page.contents).select(".index_paginare a.butonpaginare:not(.inainte)")
        .iterator().asScala.toList

      val lastButton = buttons.last
      val lastPage = PageUrlNumbered(lastButton.absUrl("href"), Integer.parseInt(lastButton.text()))

      for {
        pn <- 1 to lastPage.pageNumber
        stringUrl = lastPage.url.toString
        baseUrl = stringUrl.substring(0, stringUrl.lastIndexOf("=") + 1)
      } yield PageUrlNumbered(baseUrl + pn, pn)

    }.map(_.toList).toEither

  }


  private def parseFloor(floor: String): (Int, Int) = {
    val parts = floor.split("/")

    val maxFloor = if (parts.size > 1) Integer.parseInt(parts(1)) else 0

    if (floor.toLowerCase.contains("parter")) (0, maxFloor)
    else if (floor.toLowerCase.contains("demisol")) (-1, maxFloor)
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
      rooms = parseRoom(partsToText) ,
      surface = parseSurface(partsToText),
      floor,
      maxFloor,
      parseCompariment(partsToText(CompartimentPart)),
      partsToText.contains(IsNew)
    )

  }

  private def parseSurface(partsToText: Map[SimpleViewParts, String]) = {
    val surface = partsToText(Surface)
    Try { surface.filter(d => d.isDigit || '.'.equals(d)).toDouble.intValue()}.getOrElse(0)
  }

  private def parseRoom(partsToText: Map[SimpleViewParts, String]) : Int= {
      val rooms = partsToText(Rooms)
      if (rooms.equals("o cameră")) 1
      else Try {Integer.parseInt(rooms.filter(_.isDigit))}.getOrElse(0)
  }

  case class Parts(rooms: Int, surface: Int, floor: Int, maxFloor:Int, c: Compartiment, isNew: Boolean)


  override def simpleViews(page: PageContents): Either[Throwable, List[SimpleView]] = {
    val boxes = Try {
      val doc = Jsoup.parse(page.contents)
      doc.select(".box-anunt:not(.anunt-special)").iterator().asScala.toList
    }.toEither

    boxes.flatMap { bxs =>
      val opts = bxs.map(parseSimpleView)
      opts.flatten.asRight
    }
  }

  private def parseSimpleView(box: Element): Option[SimpleView] = {
    Try {
      val idParts = box.id().split("-")
      val id = if (idParts.size > 1) idParts(1) else "noId"
      val url = box.selectFirst(".row .mobile-container-url").absUrl("href")
      val descriptionElement = box.selectFirst(".caracteristici")
      val priceEur = Try {
        Integer.parseInt(box.select(".pret-mare").text().filter(_.isDigit))
      }.getOrElse(0)
      val parts = parseParts(descriptionElement)
      val location = parseLocation(box.selectFirst(".localizare"))
      SimpleView(id, url, parts.surface, parts.rooms, Floor(parts.floor, parts.maxFloor), parts.c,
        if (parts.isNew) NewBuilding else Year(-1), //TODO
        priceEur, location
      )
    }.toOption //TODO maybe log Failed element
  }

  def parseLocation(location: Element): String = {
    val text = location.text().toLowerCase
    val zoneIndex = text.lastIndexOf("zona")
    if (zoneIndex == -1) ""
    else text.substring(zoneIndex + 5)
  }

  override def pageContents(uRL: String): Either[Throwable, PageContents] = Try {
    import scala.concurrent.duration._
    val connection = Jsoup.connect(uRL)
        .userAgent("User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/75.0.3770.100 Safari/537.36")
        .header("Accept-Language", "en-US,en;q=0.9")
        .header("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3")

    val doc = if (!System.getProperty("http.proxyHost", "nil").equals("nil")) {
        connection
          .proxy(System.getProperty("http.proxyHost"), System.getProperty("http.proxyPort").toInt)
          .get()
    } else connection.get()

      PageContents(doc.outerHtml())
  }.toEither

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
