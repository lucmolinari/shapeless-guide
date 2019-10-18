package chap2
import shapeless.Generic

object CoproductsEncodings {

  def main(args: Array[String]) = {
    sealed trait Shape
    final case class Rectangle(width: Double, height: Double) extends Shape
    final case class Cirle(radius: Double) extends Shape

    val gen = Generic[Shape]

    println(gen.to(Rectangle(3.0, 4.0)))

    println(gen.to(Cirle(1.0)))
  }

}