package teamtechsession

import shapeless.Generic

object Two_Generic {

  case class Track(title: String, duration: Int, deleted: Boolean)

  def main(args: Array[String]): Unit = {
    val track = Track("Title 1", 100, true)
    val trackGen = Generic[Track]

    val genericRepr = trackGen.to(track)

    val track2 = trackGen.from(genericRepr)
    println(genericRepr)
    println(track2)
  }

}
