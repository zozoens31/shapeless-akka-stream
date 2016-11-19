package akka.stream.scaladsl

import akka.stream.{Inlet, Outlet, Shape}
import shapeless.PolyDefns.~>
import shapeless.ops.hlist.{Length, Prepend, Split}
import shapeless.ops.nat.ToInt
import shapeless.{::, HList, HNil, Nat, Witness}

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.Seq
import scala.language.higherKinds

/**
  * Created by cyrille on 18/11/2016.
  */
case class HListShape[-I <: HList, +O <: HList](ins: I @uncheckedVariance, outs: O @uncheckedVariance)(implicit I: AllAre[I, Inlet], O: AllAre[O, Outlet]) extends Shape{
  override def inlets: Seq[Inlet[_]] = I.toSeq(ins)

  override def outlets: Seq[Outlet[_]] = O.toSeq(outs)

  override def deepCopy(): HListShape[I, O] = HListShape(I.allApply(ins)(HListShape.inCarbonCopy), O.allApply(outs)(HListShape.outCarbonCopy))

  override def copyFromPorts(inlets: Seq[Inlet[_]], outlets: Seq[Outlet[_]]): Shape = HListShape(I.fromSeq(inlets), O.fromSeq(outlets))
}

object HListShape {
  private[scaladsl] object inCarbonCopy extends (Inlet ~> Inlet) {
    override def apply[T](f: Inlet[T]): Inlet[T] = f.carbonCopy()
  }
  private[scaladsl] object outCarbonCopy extends (Outlet ~> Outlet) {
    override def apply[T](f: Outlet[T]): Outlet[T] = f.carbonCopy()
  }
}

trait AllAre[L <: HList, F[_]] {
  type Inside <: HList
  def toSeq(l: L): Seq[F[_]]
  def allApply(l: L)(f: F ~> F): L
  def fromSeq(s: Seq[F[_]]): L
}

object AllAre extends AllAreLowerImplicits {
  type Aux[L <: HList, F[_], I <: HList] = AllAre[L, F] {
  type Inside = I
  }
  implicit def hnil[F[_]]: Aux[HNil, F, HNil] = new AllAre[HNil, F] {
    override type Inside = HNil
    override def toSeq(l: HNil): Seq[F[_]] = Seq()
    override def allApply(l: HNil)(f: ~>[F, F]): HNil = l
    override def fromSeq(s: Seq[F[_]]): HNil = {
      require(s.isEmpty, s"too many elements in Seq [${s.mkString(", ")}]")
      HNil
    }
  }
  implicit def hCons[F[_], H, T <: HList](implicit tail: T AllAre F): Aux[F[H] :: T, F, H :: tail.Inside] = new AllAre[F[H] :: T, F] {
    override type Inside = H :: tail.Inside
    override def toSeq(l: F[H] :: T): Seq[F[_]] = l.head +: tail.toSeq(l.tail)
    override def allApply(l: F[H] :: T)(f: F ~> F): F[H] :: T = f(l.head) :: tail.allApply(l.tail)(f)
    override def fromSeq(s: Seq[F[_]]): F[H] :: T = {
      require(s.nonEmpty, s"not enough elements in Seq")
      require(s.head.isInstanceOf[F[H]], s"invalid type of head element [${s.head}]")
      s.head.asInstanceOf[F[H]] :: tail.fromSeq(s.tail)
    }
  }
}

trait AllAreLowerImplicits { self: AllAre.type =>
  implicit def prepend[F[_], L0 <: HList, L1 <: HList, L <: HList, I0 <: HList, I1 <: HList, N <: Nat](implicit
                                                                                             L: Prepend.Aux[L0, L1, L],
                                                                                             LS: Split.Aux[L, N, L0, L1],
                                                                                             I: I0 Prepend I1,
                                                                                             A0: AllAre.Aux[L0, F, I0],
                                                                                             A1: AllAre.Aux[L1, F, I1],
                                                                                             l: Length.Aux[L0, N],
                                                                                             N: ToInt[N]): AllAre.Aux[L, F, I.Out] =
    new AllAre[L, F] {
      override type Inside = I.Out

      override def toSeq(l: L): Seq[F[_]] = {
        val (l0, l1) = LS(l)
        A0.toSeq(l0) ++ A1.toSeq(l1)
      }

      override def allApply(l: L)(f: ~>[F, F]): L = {
        val (l0, l1) = LS(l)
        L(A0.allApply(l0)(f), A1.allApply(l1)(f))
      }

      override def fromSeq(s: Seq[F[_]]): L = {
        val l0 = Nat.toInt[N]
        L(A0.fromSeq(s.take(l0)), A1.fromSeq(s.drop(l0)))
      }
    }

}