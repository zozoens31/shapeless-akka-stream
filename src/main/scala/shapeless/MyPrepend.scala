package shapeless

import shapeless.ops.hlist.Length
import shapeless.ops.nat.ToInt

/**
  * Created by cyrille on 20/11/2016.
  */
trait MyPrepend[A <: HList, B <: HList] {
  type Out <: HList

  def unapply(out: Out): Option[(A, B)]

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

trait LowerImplicit extends MyPrependLowestImplicits {
  implicit def rightHnil[B <: HNil, A <: HList, N <: Nat](implicit N: Length.Aux[A, N], n: ToInt[N]): MyPrepend.Aux[A, B, A] = new MyPrepend[A, B] {
    type Out = A

    override def unapply(out: A) = Some((out, HNil.asInstanceOf[B]))

    override def apply(a: A, b: B) = a

    val leftSize = n()
  }
}

trait MyPrependLowestImplicits {

  implicit def hcons[H, T <: HList, B <: HList](implicit P: T MyPrepend B): MyPrepend.Aux[H :: T, B, H :: P.Out] = new MyPrepend[H :: T, B] {
    type Out = H :: P.Out
    val leftSize = P.leftSize + 1
    val PP = P.asInstanceOf[MyPrepend.Aux[T, B, P.Out]]  //workaround for SI-9247 https://issues.scala-lang.org/browse/SI-9247

    override def apply(a: ::[H, T], b: B) = a.head :: P(a.tail, b)

    override def unapply(out: H :: P.Out) = out match {
      case h :: PP(t, b) => Some((h :: t, b))
    }
  }
}