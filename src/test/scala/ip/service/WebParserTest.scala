package ip.service

import java.net.URL

import org.scalatest.{EitherValues, FeatureSpec, Inside, Matchers}

import scala.io.Source

class WebParserTest extends  FeatureSpec with Matchers with EitherValues with Inside {

  val parser = new WebParser

  scenario("get all page urls from mainpage, starting with 2nd") {
    val rawHtml = Source.fromResource("web_pages/mainpage.htm").mkString
    val doc = PageContents(rawHtml)

    val result = parser.pageUrls(doc)
    inside(result) {
      case Right(value) =>
        value should not be empty
        value should have size 39

      case Left(ex) => fail(ex)
    }
  }

  scenario("parse boxes correctly aka simple view") {
    val rawHtml = Source.fromResource("web_pages/mainpage.htm").mkString
    val doc = PageContents(rawHtml)

    val result = parser.simpleViews(doc)

    inside(result) {
      case Left(ex) => fail(ex)
      case Right(views) =>
        views should have size 28 //29 total, one has error and it is skipped
        val last = views.dropRight(1).last

        last shouldBe SimpleView("XANP00011",
          "https://www.imobiliare.ro/vanzare-apartamente/cluj-napoca/marasti/apartament-de-vanzare-3-camere-XANP00011?lista=140934042",
          77, 3, Floor(4, 6), Decomandat, NewBuilding, 91207, "marasti")
    }
  }

  scenario("csv") {

    val s = SimpleView("id", "http://www.google.com", 23, 5, Floor(2, 5), Nedecomandat, NewBuilding, 23000, "marasti")


    val csv = new CsvService().exportToCsv(List(s))

    csv shouldBe List("id,5,23,2,5,nedecomandat,0,23000,marasti,http://www.google.com")

  }

}
