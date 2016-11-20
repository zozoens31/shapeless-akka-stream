package akka.stream
package scaladsl

import akka.actor.ActorSystem
import akka.stream.scaladsl.GraphDSL.Builder
import impl.StreamLayout.Module
import shapeless.{AllAre, HList, HNil, MyPrepend, Nat}

import scala.annotation.unchecked.uncheckedVariance

/**
  * Created by cyrille on 18/11/2016.
  */
final class HListGraph[-I0 <: HList, +O0 <: HList, +M0](override val module: Module) extends Graph[HListShape[I0, O0], M0] {
  self =>
  override val shape = module.shape.asInstanceOf[HListShape[I0, O0]]

  override def withAttributes(attr: Attributes) =
    new HListGraph[I0, O0, M0](module.withAttributes(attr))

  override def toString: String = s"HListGraph($shape, $module)"

  def stackMat[
  I1 <: HList,
  O1 <: HList,
  M1,
  M2
  ](that: Graph[HListShape[I1, O1], M1])(combine: (M0, M1) => M2)(implicit
                                                                  I: MyPrepend[I0@uncheckedVariance, I1],
                                                                  AI0: AllAre[I0@uncheckedVariance, Inlet],
                                                                  AI1: AllAre[I1, Inlet],
                                                                  O: MyPrepend[O0@uncheckedVariance, O1],
                                                                  AO0: AllAre[O0@uncheckedVariance, Outlet],
                                                                  AO1: AllAre[O1, Outlet]
  ): HListGraph[I.Out, O.Out, M2] =
    HListGraph.fromGraph(GraphDSL.create(this, that)(combine) { _ =>
      (zero, one) =>
        HListShape(I(zero.ins, one.ins), O(zero.outs, one.outs))(AllAre.prepend[Inlet, I0, I1], AllAre.prepend[Outlet, O0, O1])
    })

  def stack[I1 <: HList, O1 <: HList, M1](that: Graph[HListShape[I1, O1], M1])(implicit
                                                                               I: MyPrepend[I0@uncheckedVariance, I1],
                                                                               AI0: AllAre[I0@uncheckedVariance, Inlet],
                                                                               AI1: AllAre[I1, Inlet],
                                                                               O: MyPrepend[O0@uncheckedVariance, O1],
                                                                               AO0: AllAre[O0@uncheckedVariance, Outlet],
                                                                               AO1: AllAre[O1, Outlet]
  ): HListGraph[I.Out, O.Out, M0] = stackMat(that)(Keep.left)

  def toMat[O0n <: Nat, I1n <: Nat] = new ToMat[O0n, I1n] {
    def apply[I1 <: HList, O1 <: HList, M1, M2](that: Graph[HListShape[I1, O1], M1])(combine: (M0, M1) => M2)(implicit link: CanLink[O0@uncheckedVariance, O0n, I1, I1n, I0@uncheckedVariance, O1]): HListGraph[link.NewIn, link.NewOut, M2] =
      HListGraph.fromGraph(GraphDSL.create(self, that)(combine) { implicit builder =>
        (left, right) =>
          link(left.outs, right.ins)
          HListShape(link.ins(left.ins, right.ins), link.outs(left.outs, right.outs))(link.allAreIn, link.allAreOut)
      })
  }

  trait ToMat[O0n <: Nat, I1n <: Nat] {
    def apply[I1 <: HList, O1 <: HList, M1, M2](that: Graph[HListShape[I1, O1], M1])(combine: (M0, M1) => M2)(implicit link: CanLink[O0@uncheckedVariance, O0n, I1, I1n, I0@uncheckedVariance, O1]): HListGraph[link.NewIn, link.NewOut, M2]
  }

  def to[O0n <: Nat, I1n <: Nat] = new To[O0n, I1n] {
    def apply[I1 <: HList, O1 <: HList, M1](that: Graph[HListShape[I1, O1], M1])(implicit link: CanLink[O0@uncheckedVariance, O0n, I1, I1n, I0@uncheckedVariance, O1]): HListGraph[link.NewIn, link.NewOut, M0] =
      toMat[O0n, I1n](that)(Keep.left)
  }

  trait To[O0n <: Nat, I1n <: Nat] {
    def apply[I1 <: HList, O1 <: HList, M1](that: Graph[HListShape[I1, O1], M1])(implicit link: CanLink[O0@uncheckedVariance, O0n, I1, I1n, I0@uncheckedVariance, O1]): HListGraph[link.NewIn, link.NewOut, M0]
  }

  trait To_:[O1n <: Nat, I0n <: Nat] {
    def apply[I1 <: HList, O1 <: HList, M1](that: Graph[HListShape[I1, O1], M1])(implicit link: CanLink[O1, O1n, I0@uncheckedVariance, I0n, I1, O0@uncheckedVariance]): HListGraph[link.NewIn, link.NewOut, M0]
  }

  def to_:[O1n <: Nat, I0n <: Nat] = new To_:[O1n, I0n] {
    def apply[I1 <: HList, O1 <: HList, M1](that: Graph[HListShape[I1, O1], M1])(implicit link: CanLink[O1, O1n, I0@uncheckedVariance, I0n, I1, O0@uncheckedVariance]): HListGraph[link.NewIn, link.NewOut, M0] =
      HListGraph.fromGraph(that).toMat(self)(Keep.right)
  }

  def run()(implicit can: CanRun[I0@uncheckedVariance, O0@uncheckedVariance], mat: Materializer): M0 = can.run(this)
}

object HListGraph {
  def fromGraph[I <: HList, O <: HList, M](g: Graph[HListShape[I, O], M]): HListGraph[I, O, M] = g match {
    case s: HListGraph[I, O, M] ⇒ s
    case other ⇒ new HListGraph[I, O, M](other.module)
  }

  def fromInputsOutputsMat[I <: HList, O <: HList, M0, M1, M2](g0: Graph[HListShape[I, HNil], M0], g1: Graph[HListShape[HNil, O], M1])(combine: (M0, M1) => M2)(implicit I: I AllAre Inlet, O: O AllAre Outlet): HListGraph[I, O, M2] = fromGraph(GraphDSL.create(g0, g1)(combine) { _ =>
    (g0, g1) =>
      HListShape(g0.ins, g1.outs)
  })

}


