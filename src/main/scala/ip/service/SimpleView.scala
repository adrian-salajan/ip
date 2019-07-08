package ip.service

import java.net.URL

case class Etaj(etaj: Int, etajMax: Int)

sealed trait Compartiment
object Decomandat extends Compartiment
object Nedecomandat extends Compartiment
object Circular extends Compartiment
case class Other(compartiment: String) extends Compartiment

sealed trait Vechime
object BlocNou extends Vechime
case class An(an: Int) extends Vechime

case class SimpleView(id: String, url: URL,  mp: Int, camere: Int, etaj: Etaj, compartiment: Compartiment, vechime:Vechime)