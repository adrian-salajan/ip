package ip.service

import java.io.{File, PrintWriter}


class CsvService {

  def writeToFile(csvLines: List[String], fileName: String): Unit = {
    val writer = new PrintWriter(new File(fileName))
    writer.println(CsvService.SimpleViewHeader)
    csvLines.foreach(line => writer.println(line))
    writer.flush()
    writer.close()
  }


  def exportToCsv(simpleViews: List[SimpleView]): List[String] = {

    simpleViews.map(toCsv)

  }

  private def toCsv(s: SimpleView): String = {
    s"${s.id},${s.rooms}," +
      s"${s.surface},${s.floor.floor},${s.floor.floorMax},${compartiment(s.compartiment)},${age(s.age)}," +
      s"${s.priceEur},${s.location},${s.url.toString}"
  }


  private def compartiment(compartiment: Compartiment): String = compartiment match {
    case Nedecomandat => "nedecomandat"
    case Decomandat => "decomandat"
    case Circular => "circular"
    case Other(other) => other
  }

  private def age(age: Age): String = age match {
    case NewBuilding => "0"
    case Year(yr) => yr.toString
  }



}

object CsvService {

  val SimpleViewHeader = "id,rooms,surface,floor,maxfloor,compartiment,age,euro,location,link"


}
