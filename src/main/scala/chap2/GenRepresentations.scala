package chap2

import shapeless.Generic

object GenRepresentations {

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  case class Employee(name: String, number: Int, manager: Boolean)

  def main(args: Array[String]) = {
    val iceCreamGen = Generic[IceCream]

    val iceCream = IceCream("Sundae", 1, false)

    val iceCreamRepr = iceCreamGen.to(iceCream)
    val iceCream2 = iceCreamGen.from(iceCreamRepr)

    println(iceCreamRepr)
    println(iceCream2)

    val employee = Generic[Employee].from(iceCreamRepr)
    println(employee)
  }

}