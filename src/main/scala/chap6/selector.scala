package chap6

import shapeless._
import shapeless.record._

object Selector {

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  def main(args: Array[String]): Unit = {
    val sundae = LabelledGeneric[IceCream].to(IceCream("Sundae", 1, false))
    val name = sundae.get('name)
    val numCherries = sundae.get('numCherries)

    println(LabelledGeneric[IceCream].from(sundae.updated('numCherries, 3)))
    println(LabelledGeneric[IceCream].from(sundae.updateWith('name)("MASSIVE " + _)))
  }

}