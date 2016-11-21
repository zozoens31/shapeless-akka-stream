package shapeless

import shapeless.ops.hlist.Length
import shapeless.ops.nat.ToInt

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
  implicit def hnil[A <: HNil, B <: HList]: Aux[A, B, B] = new MyPrepend[A, B] {
    type Out = B

    override def unapply(out: B) = Some((HNil.asInstanceOf[A], out))

    override def apply(a: A, b: B) = b

    val leftSize = 0
  }


}

trait LowerImplicit extends EvenLowerImplicits {
  self: MyPrepend.type =>
  implicit def rightHnil[B <: HNil, A <: HList, N <: Nat](implicit N: Length.Aux[A, N], n: ToInt[N]): Aux[A, B, A] = new MyPrepend[A, B] {
    type Out = A

    override def unapply(out: A) = Some((out, HNil.asInstanceOf[B]))

    override def apply(a: A, b: B) = a

    val leftSize = n()
  }
}

trait EvenLowerImplicits {
  self: MyPrepend.type =>

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