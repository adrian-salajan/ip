package ip.service

import java.net.URL

case class Floor(etaj: Int, etajMax: Int)

sealed trait Compartiment
object Decomandat extends Compartiment
object Nedecomandat extends Compartiment
object Circular extends Compartiment
case class Other(compartiment: String) extends Compartiment

sealed trait Age
object NewBuilding extends Age
case class Year(year: Int) extends Age

case class SimpleView(id: String, url: URL, surface: Int, rooms: Int, etaj: Floor, compartiment: Compartiment, age:Age)