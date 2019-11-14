package teamtechsession

import CsvData._

import shapeless.{Generic, HList, ::, HNil}
import shapeless.Lazy
import shapeless.ops.record.Keys
import shapeless.LabelledGeneric
import shapeless._
import shapeless.ops.record._
import shapeless.ops.hlist.ToTraversable

object Six_CsvWithHeaderShapeless {

  import CsvEncoder._

  case class User(id: Int, name: String)

  def main(args: Array[String]): Unit = {
    // val r = Generic[Track]

    println(writeCsv(List(track1, track2)))
    println(writeCsv(List(User(1, "user1"), User(2, "user2"))))
  }

  def writeCsv[A](values: List[A])(
      implicit
      enc: CsvEncoder[A],
      fields: FieldNames[A]
  ): String =
    fields().mkString(",") + "\n" + values
      .map(v => enc.encode(v).mkString(","))
      .mkString("\n")

  trait CsvEncoder[A] {
    def encode(value: A): List[String]
  }

  trait FieldNames[T] {
    def apply(): List[String]
  }

  object CsvEncoder {

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

    implicit val hnilEncoder: CsvEncoder[HNil] = createEncoder(_ => Nil)

    implicit def hlistEncoder[H, T <: HList](
        implicit
        hEncoder: Lazy[CsvEncoder[H]],
        tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :: T] = createEncoder {
      case h :: t =>
        hEncoder.value.encode(h) ++ tEncoder.encode(t)
    }

    // Given a type A and an HList type R, an implicit Generic to map A to R, and a CsvEncoder for R, create a CsvEncoder for A.
    implicit def genericEncoder[A, R](
        implicit
        gen: Generic.Aux[A, R],
        enc: CsvEncoder[R]
    ): CsvEncoder[A] = createEncoder(a => enc.encode(gen.to(a)))

    implicit def toNames[T, Repr <: HList, KeysRepr <: HList](
        implicit
        gen: LabelledGeneric.Aux[T, Repr],
        keys: Keys.Aux[Repr, KeysRepr],
        traversable: ToTraversable.Aux[KeysRepr, List, Symbol]
    ): FieldNames[T] = new FieldNames[T] {
      def apply() = keys().toList.map(_.name)
    }

  }

}
