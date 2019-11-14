package teamtechsession

import shapeless.Generic

object Three_GenericCoproducts {

  sealed trait Failure
  case class SoftFailure(msg: String, attempts: Int) extends Failure
  case class HardFailure(msg: String) extends Failure

  def main(args: Array[String]): Unit = {
    // :+: = OR
    // Inl = "left"; Inr = "right"
    // We can’t instanciate CNil or build a Coproduct purely from instances of Inr. We always have exactly one Inl in a value.
    val gen = Generic[Failure]

    val s = gen.to(SoftFailure("Error message", 10))
    val h = gen.to(HardFailure("Hard error"))

    println(s)
    println(h)
  }

}
