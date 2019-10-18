package chap4

import shapeless.{::, HList, HNil}
import shapeless.Generic
import shapeless.ops.hlist.Last
import shapeless.ops.hlist.IsHCons

trait Second[L <: HList] {
  type Out
  def apply(value: L): Out
}

object Second {
  type Aux[L <: HList, O] = Second[L] { type Out = O }

  def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] = inst
}

object SecondRunner {

  import Second._

  implicit def hlistSecond[A, B, Rest <: HList]: Aux[A :: B :: Rest, B] =
    new Second[A :: B :: Rest] {
      type Out = B
      def apply(value: A :: B :: Rest): B = value.tail.head
    }

  def lastField[A, Repr <: HList](input: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    last: Last[Repr]
  ): last.Out = last.apply(gen.to(input))

  def getWrappedValue[A, Repr <: HList, Head](input: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    isHCons: IsHCons.Aux[Repr, Head, HNil]
  ): Head = gen.to(input).head

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  case class Wrapper(value: Int)

  def main(args: Array[String]) = {
    val second1 = Second[String :: Boolean :: Int :: HNil]
    val second2 = Second[String :: Int :: Boolean :: HNil]
    println(second1("foo" :: true :: 1 :: HNil))
    println(second2("foo" :: 1 :: false :: HNil))

    // Second[String :: HNil]
    println("==> Last field")
    println(lastField(IceCream("Sundae", 10, false)))

    println("==> Value")
    println(getWrappedValue(Wrapper(42)))
  }

}
