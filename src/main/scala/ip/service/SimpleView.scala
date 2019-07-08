package ip.service


case class Floor(floor: Int, floorMax: Int)

sealed trait Compartiment
object Decomandat extends Compartiment
object Nedecomandat extends Compartiment
object Circular extends Compartiment
case class Other(compartiment: String) extends Compartiment

sealed trait Age
object NewBuilding extends Age
case class Year(year: Int) extends Age

case class SimpleView(
                       id: String,
                       url: String,
                       surface: Int,
                       rooms: Int,
                       floor: Floor,
                       compartiment: Compartiment,
                       age:Age,
                       priceEur: Int)