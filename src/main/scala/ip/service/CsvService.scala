package ip.service

import java.io.{File, PrintWriter}

import purecsv.safe.converter.StringConverter

import scala.util.Try



class CsvService {


  def exportToCsv(simpleViews: List[SimpleView]): List[String] = {
    import purecsv.safe._
    simpleViews.toCSVLines(",").toList
  }

  def writeToFile(csvLines: List[String], fileName: String): Unit = {
    val writer = new PrintWriter(new File(fileName))
    writer.println(CsvService.SimpleViewHeader)
    csvLines.foreach(line => writer.println(line))
    writer.flush()
    writer.close()

  }

}

object CsvService {

  val SimpleViewHeader = "id,surface,rooms,floor,maxfloor,compartiment,age"

  implicit val floorConverter: StringConverter[Floor] = new StringConverter[Floor] {

    override def tryFrom(b: String): Try[Floor] = Try {
      val parts = b.split(",")
      Floor(parts(0).toInt, parts(1).toInt)
    }

    override def to(a: Floor): String = s"${a.floor},${a.floorMax}"
  }

  implicit val compartimentConverter: StringConverter[Compartiment] = new StringConverter[Compartiment] {

    override def tryFrom(b: String): Try[Compartiment] = Try {
      b match {
        case "decomandat" => Decomandat
        case "nedecomandat" => Nedecomandat
        case "circular" => Circular
        case other => Other(other)
      }
    }

    override def to(a: Compartiment): String = a match {
      case Decomandat => "decomandat"
      case Nedecomandat => "nedecomandat"
      case Circular => "circular"
      case Other(other) => other
    }
  }

  implicit val ageConverter: StringConverter[Age] = new StringConverter[Age] {

    override def tryFrom(b: String): Try[Age] = Try {
      b match {
        case "0" | "nou" => NewBuilding
        case digits => Year(digits.toInt)
      }
    }

    override def to(a: Age): String = a match {
      case NewBuilding => "0"
      case Year(yr) => yr.toString
    }
  }

}
