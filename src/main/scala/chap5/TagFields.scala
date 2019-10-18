package chap5

import shapeless.labelled.{field, KeyTag, FieldType}
import shapeless.syntax.singleton._
import shapeless.Witness

object TagFields {

  trait Cherries

  def getFieldName[K, V](value: FieldType[K, V])(implicit witness: Witness.Aux[K]): K = witness.value

  def getFieldValue[K, V](value: FieldType[K, V]): V = value

  def main(args: Array[String]): Unit = {
    val someNumber = 123
    val numCherries = "numCherries" ->> someNumber
    println(numCherries)

    println(field[Cherries](123))

    println(getFieldName(numCherries))
    println(getFieldValue(numCherries))
  }

}