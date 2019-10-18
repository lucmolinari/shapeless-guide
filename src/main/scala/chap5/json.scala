package chap5

import shapeless.{LabelledGeneric, HList, ::, HNil, Coproduct, :+:, CNil, Inl, Inr, Witness, Lazy}
import shapeless.labelled.{field, KeyTag, FieldType}

object JsonRunner {

  import JsonHelper._

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape  

  def main(args: Array[String]): Unit = {
    val iceCream = IceCream("Sundae", 1, false)

    val gen = LabelledGeneric[IceCream].to(iceCream)

    println(JsonEncoder[IceCream].encode(iceCream))

    val shape = Circle(1.0)
    LabelledGeneric[Shape].to(shape)
    println(JsonEncoder[Shape].encode(shape))
  }

}

sealed trait JsonValue
case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue
case class JsonArray(items: List[JsonValue]) extends JsonValue
case class JsonString(value: String) extends JsonValue
case class JsonNumber(value: Double) extends JsonValue
case class JsonBoolean(value: Boolean) extends JsonValue
case object JsonNull extends JsonValue

trait JsonEncoder[A] {
  def encode(value: A): JsonValue
}

object JsonEncoder {
  def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc
}

object JsonHelper {

  def createEncoder[A](func: A => JsonValue): JsonEncoder[A] =
    new JsonEncoder[A] {
      def encode(value: A): JsonValue = func(value)
    }

  implicit val stringEncoder: JsonEncoder[String] = createEncoder(JsonString)

  implicit val doubleEncoder: JsonEncoder[Double] = createEncoder(JsonNumber)

  implicit val intEncoder: JsonEncoder[Int] = createEncoder(JsonNumber(_))

  implicit val booleanEncoder: JsonEncoder[Boolean] = createEncoder(JsonBoolean)

  implicit def listEncoder[A](
      implicit enc: JsonEncoder[A]
  ): JsonEncoder[List[A]] =
    createEncoder(list => JsonArray(list.map(enc.encode)))

  implicit def optionEncoder[A](
      implicit enc: JsonEncoder[A]
  ): JsonEncoder[Option[A]] =
    createEncoder(opt => opt.map(enc.encode).getOrElse(JsonNull))

  def createObjectEncoder[A](fn: A => JsonObject): JsonObjectEncoder[A] =
    new JsonObjectEncoder[A] {
      def encode(value: A): JsonObject = fn(value)
    }

  implicit val hnilEncoder: JsonObjectEncoder[HNil] =
    createObjectEncoder(_ => JsonObject(Nil))

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](
      implicit
      witness: Witness.Aux[K],
      hEncoder: Lazy[JsonEncoder[H]],
      tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name
    createObjectEncoder { hList =>
      val head = hEncoder.value.encode(hList.head)
      val tail = tEncoder.encode(hList.tail)
      JsonObject((fieldName, head) :: tail.fields)
    }
  }

  implicit def genericObjectEncoder[A, H](
      implicit
      generic: LabelledGeneric.Aux[A, H],
      hEncoder: Lazy[JsonObjectEncoder[H]]
  ): JsonEncoder[A] =
    createObjectEncoder { value =>
      hEncoder.value.encode(generic.to(value))
    }

  implicit val cnilObjectEncodder: JsonObjectEncoder[CNil] = 
  createObjectEncoder(_ => throw new Exception("Not possible"))

  implicit def coproductObjectEncoder[K <: Symbol, H, T <: Coproduct](
    implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[JsonEncoder[H]],
    tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :+: T] = {
    val typeName = witness.value.name
    createObjectEncoder {
      case Inl(h) => JsonObject(List(typeName -> hEncoder.value.encode(h)))
      case Inr(t) => tEncoder.encode(t)
    }
  }

}

trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  def encode(value: A): JsonObject
}
