package chap3

object CSVEncoderRunner {

  def writeCSV[A](values: List[A])(implicit enc: CSVEncoder[A]): String = {
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    val employees = List(
      Employee("Bill", 1, true),
      Employee("Peter", 2, false),
      Employee("Milton", 3, false)
    )
    println("===> CSV Employees")
    println(writeCSV(employees))

    val iceCreams = List(
      IceCream("Sundae", 1, false),
      IceCream("Cornetto", 0, true),
      IceCream("Banana Split", 0, false)
    )
    println("===> CSV Ice Creams")
    println(writeCSV(iceCreams))

    println("===> CSV Employees/Ice Creams")
    println(writeCSV(employees zip iceCreams))
  }

  trait CSVEncoder[A] {
    def encode(value: A): List[String]
  }

  case class Employee(name: String, number: Int, manager: Boolean)

  implicit val employeeEncoder: CSVEncoder[Employee] =
    new CSVEncoder[Employee] {
      def encode(e: Employee): List[String] =
        List(e.name, e.number.toString, if (e.manager) "yes" else "no")
    }

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  implicit val iceCreamEncoder: CSVEncoder[IceCream] =
    new CSVEncoder[IceCream] {
      def encode(i: IceCream): List[String] =
        List(i.name, i.numCherries.toString, if (i.inCone) "yes" else "no")
    }

  implicit def pairEncoder[A, B](
      implicit
      aEncoder: CSVEncoder[A],
      bEncoder: CSVEncoder[B]
  ): CSVEncoder[(A, B)] =
    new CSVEncoder[(A, B)] {
      def encode(pair: (A, B)): List[String] = {
        val (a, b) = pair
        aEncoder.encode(a) ++ bEncoder.encode(b)
      }
    }

}
