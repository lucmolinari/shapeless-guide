package teamtechsession

import shapeless._
import shapeless.record._
import CsvData._

object Seven_AccessingFields {

  def main(args: Array[String]): Unit = {
    val repr = LabelledGeneric[Track].to(track1)

    val title = repr.get('title)

  }

}