package teamtechsession

import shapeless.{HList, ::, HNil}

object One_HList {

  val product = "One String" :: 10 :: true :: HNil
  val str = product.head
  val headTail = product.tail.head
  val tail = product.tail

}
