package chap3

import shapeless.{HList, ::, HNil}
import shapeless.Generic

object CsvHLists {

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

  implicit val hnilEncoder: CsvEncoder[HNil] = createEncoder(hnil => Nil)

  implicit def hlistEncoder[H, T <: HList](
      implicit
      hEncoder: CsvEncoder[H],
      tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] = createEncoder {
    case h :: t =>
      hEncoder.encode(h) ++ tEncoder.encode(t)
  }

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  val iceCreams = List(
    IceCream("Sundae", 1, false),
    IceCream("Cornetto", 0, true),
    IceCream("Banana Split", 0, false)
  )

  case class Employee(name: String, number: Int, manager: Boolean, score: Double)

  val employees = List(
    Employee("Bill", 1, true, 1.0),
    Employee("Peter", 2, false, 2.4),
    Employee("Milton", 3, false, 3.2)
  )

  // implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
  //   val gen = Generic[IceCream]
  //   val enc = CsvEncoder[gen.Repr]
  //   createEncoder(iceCream => enc.encode(gen.to(iceCream)))
  // }

  // Given a type A and an HList type R, an implicit Generic to map A to R, 
  // and a CsvEncoder for R, create a CsvEncoder for A.
  implicit def genericEncoder[A, R](
    implicit
    // gen: Generic[A] { type Repr = R },
    gen: Generic.Aux[A, R],
    enc: CsvEncoder[R]
  ): CsvEncoder[A] = createEncoder(a => enc.encode(gen.to(a)))

  def writeCSV[A](values: List[A])(implicit enc: CsvEncoder[A]): String = {
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")
  }  

  def main(args: Array[String]) = {
    val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly

    println(reprEncoder.encode("abc" :: 123 :: true :: HNil))

    println("==> Ice creams")
    println(writeCSV(iceCreams))

    println("==> Employees")
    println(writeCSV(employees))
  }

}
