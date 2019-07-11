package ip.service

import java.io.{File, PrintWriter}


class CsvService {

  def writeToFile(csvLines: List[String], fileName: String): Unit = {
    val file = new File(fileName)

    if (!file.getParentFile.exists()) {
      file.getParentFile.mkdirs()
    }

    val writer = new PrintWriter(file)
    writer.println(CsvService.SimpleViewHeader)
    csvLines.foreach(line => writer.println(line))
    writer.flush()
    writer.close()
  }


  def exportToCsv(simpleViews: List[SimpleView]): List[String] = {

    simpleViews.zipWithIndex.map(t => toCsv(t._2, t._1))

  }

//  val SimpleViewHeader = "ordinal,id,location,euro,surface,compartiment,link"
  private def toCsv(index:Int, s: SimpleView): String = {
    s"$index,${s.id},${s.location},${s.priceEur},${s.surface},${compartiment(s.compartiment)},${s.url.toString}"
  }

//  private def toCsv(s: SimpleView): String = {
//    s"${s.id},${s.rooms}," +
//      s"${s.surface},${s.floor.floor},${s.floor.floorMax},${compartiment(s.compartiment)},${age(s.age)}," +
//      s"${s.priceEur},${s.location},${s.url.toString}"
//  }


  private def compartiment(compartiment: Compartiment): String = compartiment match {
    case Nedecomandat => "nedecomandat"
    case Decomandat => "decomandat"
    case Semidecomandat => "semidecomandat"
    case Circular => "circular"
    case Other(other) => other
  }

  private def age(age: Age): String = age match {
    case NewBuilding => "0"
    case Year(yr) => yr.toString
  }



}

object CsvService {

  val SimpleViewHeader = "ordinal,id,location,euro,surface,compartiment,link"


}
