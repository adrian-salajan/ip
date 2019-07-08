package ip.service

import java.net.URL

import org.scalatest.{EitherValues, FeatureSpec, Inside, Matchers}

import scala.io.Source

class WebParserTest extends  FeatureSpec with Matchers with EitherValues with Inside {


  val parser = new WebParser


  scenario("get all page urls from mainpage") {
    val rawHtml = Source.fromResource("web_pages/mainpage.htm").mkString
    val doc = PageContents(rawHtml)

    val result = parser.pages(doc)
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

    val result = parser.simpleView(doc)

    inside(result) {
      case Left(ex) => fail(ex)
      case Right(views) =>
        views should have size 29
        val last = views.dropRight(1).last

        last shouldBe SimpleView("XANP00011",
          new URL("https://www.imobiliare.ro/vanzare-apartamente/cluj-napoca/marasti/apartament-de-vanzare-3-camere-XANP00011?lista=140934042"),
          77, 3, Floor(4, 6), Decomandat, NewBuilding)

    }
  }

}
