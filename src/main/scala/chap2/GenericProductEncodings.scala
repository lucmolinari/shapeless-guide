package chap2

import shapeless.{HList, ::, HNil}

object GenericProductEncodings {

  def main(args: Array[String]) {
    val product: String :: Int :: Boolean :: HNil = "Sunday" :: 1 :: false :: HNil

    println(product.head)

    println(product.tail.head)

    val newProduct = 42L :: product
    println(newProduct.head)
  }

}