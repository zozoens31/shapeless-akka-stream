package shapeless

import shapeless.{::, HList, HNil}

/**
  * Created by cyrille on 20/11/2016.
  */
trait MyPrepend[A <: HList, B <: HList] {
  type Out <: HList

  def unapply(out: Out): Some[(A, B)]

  def apply(a: A, b: B): Out

  def leftSize: Int
}

object MyPrepend extends LowerImplicit {
  type Aux[A <: HList, B <: HList, O] = MyPrepend[A, B] {
    type Out = O
  }

  def hcons[H, T <: HList, B <: HList](implicit P: T MyPrepend B): Aux[H :: T, B, H :: P.Out] = new MyPrepend[H :: T, B] {
    type Out = H :: P.Out
    val leftSize = P.leftSize + 1

    override def apply(a: ::[H, T], b: B) = a.head :: P(a.tail, b)

    override def unapply(out: H :: P.Out) = {
      val (t, b) = P.unapply(out.tail).get
      Some((out.head :: t, b))
    }
  }
}

trait LowerImplicit {
  self: MyPrepend.type =>
  implicit def hnil[A <: HNil, B <: HList]: Aux[A, B, B] = new MyPrepend[A, B] {
    type Out = B

    override def unapply(out: B) = Some((HNil.asInstanceOf[A], out))

    override def apply(a: A, b: B) = b

    val leftSize = 0
  }
}