package ip.service

import org.jsoup.Jsoup
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

}
