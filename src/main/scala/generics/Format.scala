package generics

import play.api.data.validation.ValidationError
import play.api.libs.json._
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, LabelledTypeClass, LabelledTypeClassCompanion}

/**
  * Created by cyrille on 03/01/2017.
  */
trait Format[T] extends play.api.libs.json.Format[T]

object Format extends LabelledTypeClassCompanion[Format] {
  implicit def fromBasics[T](implicit T: play.api.libs.json.Format[T]) = new Format[T] {
    override def writes(o: T): JsValue = T.writes(o)

    override def reads(json: JsValue): JsResult[T] = T.reads(json)
  }

  implicit def fromEnum[T](implicit T: Values[T]) = new Format[T] {
    override def writes(o: T): JsValue = JsString(o.toString)

    override def reads(json: JsValue): JsResult[T] =
      json.validate[String].
        map(str => T.values.find(_.toString == str)).
        collect(ValidationError("invalid value in enum")){
          case Some(t) => t
        }
  }

  object typeClass extends LabelledTypeClass[Format] {
    override def coproduct[L, R <: Coproduct](name: String, cl: =>Format[L], cr: =>Format[R]): Format[:+:[L, R]] = new Format[L :+: R] {
      override def writes(o: :+:[L, R]): JsValue = o.eliminate(cl.writes, cr.writes)

      override def reads(json: JsValue): JsResult[:+:[L, R]] =
        cl.reads(json).map(Inl(_)).orElse(cr.reads(json).map(Inr(_)))
    }

    override def emptyCoproduct: Format[CNil] = new Format[CNil] {
      override def reads(json: JsValue): JsResult[CNil] = JsError("no suitable way to read this")

      override def writes(o: CNil): JsValue = JsNull
    }

    override def product[H, T <: HList](name: String, ch: Format[H], ct: Format[T]): Format[::[H, T]] = new Format[H :: T] {
      override def writes(o: ::[H, T]): JsValue = Json.obj(name -> ch.writes(o.head)) ++ ct.writes(o.tail).as[JsObject]

      override def reads(json: JsValue): JsResult[::[H, T]] = for {
        h <- (json \ name).validate(ch)
        t <- json.validate(ct)
      } yield h :: t
    }

    override def emptyProduct: Format[HNil] = new Format[HNil] {
      override def writes(o: HNil): JsValue = Json.obj()

      override def reads(json: JsValue): JsResult[HNil] = JsSuccess(HNil)
    }

    override def project[F, G](instance: => Format[G], to: (F) => G, from: (G) => F): Format[F] = new Format[F] {
      override def writes(o: F): JsValue = instance.writes(to(o))

      override def reads(json: JsValue): JsResult[F] = instance.reads(json).map(from)
    }
  }
}