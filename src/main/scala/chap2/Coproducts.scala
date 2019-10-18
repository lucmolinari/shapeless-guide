package chap2

import shapeless.{Coproduct, :+:, CNil, Inl, Inr}

object Coproducts {

  def main(args: Array[String]) = {
    case class Red()
    case class Amber()
    case class Green()

    type Light = Red :+: Amber :+: Green :+: CNil

    val red: Light = Inl(Red())

    val green: Light = Inr(Inr(Inl(Green())))

  }

}