package teamtechsession

import CsvData._

object Four_CsvNoHeader {

   case class User(id: Int, name: String)

  def main(args: Array[String]): Unit = {
    println(writeCsv(List(track1, track2)))

     println(writeCsv(List(User(1, "user1"), User(2, "user2"))))
  }

  trait CsvEncoder[A] {
    def encode(value: A): List[String]
  }

  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
    values.map(v => enc.encode(v).mkString(",")).mkString("\n")

  implicit val trackEncoder: CsvEncoder[Track] = new CsvEncoder[Track] {
    def encode(t: Track): List[String] =
      List(
        t.title,
        t.duration.toString(),
        if (t.deleted) "t" else "f",
        t.artist.name,
        t.artist.age.toString()
      )
  }

  implicit val userEncoder: CsvEncoder[User] = new CsvEncoder[User] {
    def encode(value: User): List[String] =
      List(value.id.toString(), value.name)
  }

}
