package akka.stream.scaladsl

import akka.stream.{Inlet, Outlet, Shape}
import shapeless.PolyDefns.~>
import shapeless.ops.hlist.Prepend
import shapeless.{::, HList, HNil}

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.Seq
import scala.language.higherKinds

/**
  * Created by cyrille on 18/11/2016.
  */
case class HListShape[-I <: HList, +O <: HList](ins: I @uncheckedVariance, outs: O @uncheckedVariance)(implicit I: AllAre[Inlet, I], O: AllAre[Outlet, O]) extends Shape{
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

trait AllAre[F[_], L <: HList] {
  type Inside <: HList
  def toSeq(l: L): Seq[F[_]]
  def allApply(l: L)(f: F ~> F): L
  def fromSeq(s: Seq[F[_]]): L
}

object AllAre extends {
  type Aux[F[_], L <: HList, I <: HList] = AllAre[F, L] {
  type Inside = I
  }
  implicit def hnil[F[_]]: Aux[F, HNil, HNil] = new AllAre[F, HNil] {
    override type Inside = HNil
    override def toSeq(l: HNil): Seq[F[_]] = Seq()
    override def allApply(l: HNil)(f: F ~> F): HNil = HNil
    override def fromSeq(s: Seq[F[_]]): HNil = {
      require(s.isEmpty, s"too many elements in Seq [${s.mkString(", ")}]")
      HNil
    }
  }
  implicit def hCons[F[_], H, T <: HList](implicit tail: AllAre[F, T]): Aux[F, F[H] :: T, H :: tail.Inside] = new AllAre[F, F[H] :: T] {
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

trait PrependAllAre[F[_], L0 <: HList, L1 <: HList] {
  type Out <: HList
  type InsideOut <: HList
  implicit def allAre: AllAre.Aux[F, Out, InsideOut]
}

object PrependAllAre {
  type Aux[F[_], L0 <: HList, L1 <: HList, O <: HList, IO <: HList] = PrependAllAre[F, L0, L1] {
    type Out = O
    type InsideOut = IO
  }
  implicit def hnil[F[_], L1 <: HList](implicit L1: AllAre[F, L1]): Aux[F, HNil, L1, L1, L1.Inside] = new PrependAllAre[F, HNil, L1] {
    type Out = L1
    type InsideOut = L1.Inside

    override def allAre: AllAre[F, L1] = L1
  }

  implicit def hcons[F[_], H0, T0 <: HList, L1 <: HList](implicit L1: AllAre[F, L1], L0: AllAre[F, F[H0] :: T0], P: PrependAllAre[F, T0, L1]): Aux[F, F[H0] :: T0, L1, F[H0] :: P.Out, H0 :: P.InsideOut] =
    new PrependAllAre[F, F[H0] :: T0, L1] {
      type Out = F[H0] :: P.Out
      type InsideOut = H0 :: P.InsideOut

      override def allAre: AllAre[F, F[H0] :: P.Out] = AllAre.hCons[F, H0, P.Out](P.allAre)
    }
}