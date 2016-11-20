package akka.stream.scaladsl

import akka.stream.{Inlet, Outlet, Shape}
import shapeless.{AllAre, HList}
import shapeless.PolyDefns.~>

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.Seq
import scala.language.higherKinds

/**
  * Created by cyrille on 18/11/2016.
  */
case class HListShape[-I <: HList, +O <: HList](ins: I@uncheckedVariance, outs: O@uncheckedVariance)(implicit I: AllAre[I, Inlet], O: AllAre[O, Outlet]) extends Shape {
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
