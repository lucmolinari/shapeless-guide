package chap6

import shapeless._
import shapeless.ops.hlist

object MigrationRunner {
  import Helper._

  def main(args: Array[String]): Unit = {
    println(IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2a])
    println(IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2b])
  }

}

case class IceCreamV1(name: String, numCherries: Int, inCone: Boolean)

// Remove fields:
case class IceCreamV2a(name: String, inCone: Boolean)

// Reorder fields:
case class IceCreamV2b(name: String, inCone: Boolean, numCherries: Int)

// Insert fields (provided we can determine a default value):
case class IceCreamV2c(
    name: String,
    inCone: Boolean,
    numCherries: Int,
    numWaffles: Int
)

trait Migration[A, B] {
  def apply(a: A): B
}

object Helper {

  implicit class MigrationOps[A](a: A) {
    def migrateTo[B](implicit migration: Migration[A, B]): B =
      migration.apply(a)
  }

  // Only for removed fields
  // implicit def genericMigration[A, B, ARepr <: HList, BRepr <: HList](
  //   implicit
  //   aGen: LabelledGeneric.Aux[A, ARepr],
  //   bGen: LabelledGeneric.Aux[B, BRepr],
  //   inter: hlist.Intersection.Aux[ARepr, BRepr, BRepr]
  // ): Migration[A, B] = new Migration[A, B] {
  //   def apply(a: A): B = bGen.from(inter.apply(aGen.to(a)))
  // }

  implicit def genericMigration[
      A,
      B,
      ARepr <: HList,
      BRepr <: HList,
      Unaligned <: HList
  ](
      implicit
      aGen: LabelledGeneric.Aux[A, ARepr],
      bGen: LabelledGeneric.Aux[B, BRepr],
      inter: hlist.Intersection.Aux[ARepr, BRepr, Unaligned],
      align: hlist.Align[Unaligned, BRepr]
  ): Migration[A, B] = new Migration[A, B] {
    def apply(a: A): B = 
      bGen.from(align.apply(inter.apply(aGen.to(a))))
  }

}
