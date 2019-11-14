package teamtechsession

object CsvData {

  case class Artist(name: String, age: Int)
  case class Track(
      title: String,
      duration: Int,
      deleted: Boolean,
      artist: Artist
  )

  val track1 = Track("Title 1", 500, false, Artist("Some Artist", 35))
  val track2 = Track("Title 2", 500, false, Artist("Some Artist", 20))

}
