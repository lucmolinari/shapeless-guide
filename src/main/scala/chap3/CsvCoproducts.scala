package chap3

import shapeless.{Coproduct, :+:, CNil, Inl, Inr, HNil, HList, ::, Generic, Lazy}

object CsvCoproducts {

  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  trait CsvEncoder[A] {
    def encode(value: A): List[String]
  }

  object CsvEncoder {

    // "Summoner" method
    def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

    // // "Constructor" method
    // def instance[A](func: A => List[String]): CsvEncoder[A] =
    //   new CsvEncoder[A] {
    //     def encode(value: A): List[String] = func(value)
    //   }

  }

  def createEncoder[A](func: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      def encode(value: A): List[String] = func(value)
    }

  implicit val stringEncoder: CsvEncoder[String] = createEncoder(
    str => List(str)
  )

  implicit val intEncoder: CsvEncoder[Int] = createEncoder(
    num => List(num.toString)
  )

  implicit val dubleEncoder: CsvEncoder[Double] = createEncoder(
    num => List(num.toString)
  )

  implicit val booleanEncoder: CsvEncoder[Boolean] = createEncoder(
    bool => List(if (bool) "yes" else "no")
  )

  implicit val cnilEncoder: CsvEncoder[CNil] = createEncoder(
    cnil => throw new RuntimeException("Not possible!")
  )

  implicit val hnilEncoder: CsvEncoder[HNil] = createEncoder(hnil => Nil)

  implicit def hlistEncoder[H, T <: HList](
      implicit
      hEncoder: Lazy[CsvEncoder[H]],
      tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] = createEncoder {
    case h :: t =>
      hEncoder.value.encode(h) ++ tEncoder.encode(t)
  }

  implicit def coproductEncoder[H, T <: Coproduct](
      implicit
      hEncoder: Lazy[CsvEncoder[H]],
      tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :+: T] = createEncoder {
    case Inl(h) => hEncoder.value.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

  implicit def genericEncoder[A, R](
    implicit
    gen: Generic.Aux[A, R],
    enc: Lazy[CsvEncoder[R]]
  ): CsvEncoder[A] = createEncoder(a => enc.value.encode(gen.to(a)))

  def writeCSV[A](values: List[A])(implicit enc: CsvEncoder[A]): String = {
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")
  }

  sealed trait Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] 
  case class Leaf[A](value: A) extends Tree[A]  

  def main(args: Array[String]) = {
    val shapes: List[Shape] = List(
      Rectangle(3.0, 4.0),
      Circle(1.0)
    )
    println(writeCSV(shapes))

    val trees: List[Tree[String]] = List(
      Branch(Leaf("A"), Branch(Leaf("B"), Leaf("C"))),
      Branch(Leaf("D"), Leaf("E"))
    )
    println(writeCSV(trees))
  }

}
