package teamtechsession

import CsvData._

import shapeless.{Generic, HList, ::, HNil}
import shapeless.Lazy
import java.time.LocalDate

object Five_CsvNoHeaderShapeless {

  import CsvEncoder._

  case class User(id: Int, name: String)
  
  case class Report(date: LocalDate, views: Int)

  def main(args: Array[String]): Unit = {
    println(writeCsv(List(track1, track2)))

    println(writeCsv(List(User(1, "user1"), User(2, "user2"))))

    println(writeCsv(List(Report(LocalDate.now(), 100))));
  }

  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
    values.map(v => enc.encode(v).mkString(",")).mkString("\n")

  trait CsvEncoder[A] {
    def encode(value: A): List[String]
  }

  object CsvEncoder {

    def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

    def createEncoder[A](func: A => List[String]): CsvEncoder[A] =
      new CsvEncoder[A] {
        override def encode(value: A): List[String] = func(value)
      }

    implicit val stringEncoder: CsvEncoder[String] = createEncoder(
      str => List(str)
    )

    implicit val intEncoder: CsvEncoder[Int] = createEncoder(
      num => List(num.toString)
    )

    implicit val booleanEncoder: CsvEncoder[Boolean] = createEncoder(
      bool => List(if (bool) "y" else "n")
    )

    implicit val dateEncoder: CsvEncoder[LocalDate] = createEncoder(
      date => List(date.toString())
    )

    // shapeless specific
    implicit val hnilEncoder: CsvEncoder[HNil] = createEncoder(_ => Nil)

    implicit def hlistEncoder[H, T <: HList](
        implicit
        hEncoder: Lazy[CsvEncoder[H]],
        tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :: T] = createEncoder {
      case h :: t =>
        hEncoder.value.encode(h) ++ tEncoder.encode(t)
    }

    // implicit val artistEncoder: CsvEncoder[Artist] = {
    //   val gen = Generic[Artist]
    //   val enc = CsvEncoder[gen.Repr]
    //   createEncoder(artist => enc.encode(
    //     gen.to(artist)
    //   ))
    // }

    // implicit val trackEncoder: CsvEncoder[Track] = {
    //   val gen = Generic[Track]
    //   val enc = CsvEncoder[gen.Repr]
    //   createEncoder(track => enc.encode(gen.to(track)))
    // }

    // Given a type A and an HList type R, an implicit Generic to map A to R, and a CsvEncoder for R, create a CsvEncoder for A.
    implicit def genericEncoder[A, R](
        implicit
        gen: Generic.Aux[A, R],
        enc: CsvEncoder[R]
    ): CsvEncoder[A] = createEncoder(a => enc.encode(gen.to(a)))

  }

}
