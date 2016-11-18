package akka.stream
package scaladsl

import impl.StreamLayout.Module
import shapeless.ops.hlist.Prepend
import shapeless.{::, HList, Nat, _0}

/**
  * Created by cyrille on 18/11/2016.
  */
final class HListGraph[-I0 <: HList, +O0 <: HList, +M0](override val module: Module) extends Graph[HListShape[I0, O0], M0] {
  override val shape = module.shape.asInstanceOf[HListShape[I0, O0]]

  override def withAttributes(attr: Attributes) =
    new HListGraph[I0, O0, M0](module.withAttributes(attr))

  override def toString: String = s"HListGraph($shape, $module)"

  def stackMat[I1 <: HList, O1 <: HList, M1, M2](that: Graph[HListShape[I1, O1], M1])(combine: (M0, M1) => M2)(implicit I: Prepend[I0, I1], O: Prepend[O0, O1]) =
    HListGraph.fromGraph(GraphDSL.create(this, that)(combine){ _ => (zero, one) =>
      HListShape(I(zero.ins, one.ins), O(zero.outs, one.outs))
  })

  def stack[I1 <: HList, O1 <: HList]
  def toMat[O0n <: Nat, I1n <: Nat, H, I1, O1, M1, M2](that: Graph[HListShape[I1, O1], M1])(combine: (M0, M1) => M2)(implicit out: Can[H, O0, O0n, I1, I1n, I0, O1]): HListGraph[out.NewIn, out.NewOut, M2] = ???

}

object HListGraph {
  def fromGraph[I <: HList, O <: HList, M](g: Graph[HListShape[I, O], M]): HListGraph[I, O, M] = g match {
    case s: HListGraph[I, O, M]         ⇒ s
    case other                   ⇒ new HListGraph[I, O, M](other.module)
  }

}

trait Can[H, O <: HList, On <: Nat, I <: HList, In <: Nat, OldIn <: HList, OldOut <: HList] {
  type NewIn <: HList
  type NewOut <: HList
}

object Can {
  type Aux[H, O <: HList, On <: Nat, I <: HList, In <: Nat, OldIn <: HList, OldOut <: HList, NewI <: HList, NewO <: HList] = Can[H, O, On, I, In, OldIn, OldOut] {
    type NewIn = NewI
    type NewOut = NewO
  }
  implicit def head[H, TO <: HList, TI <: HList, OldIn <: HList, OldOut <: HList](implicit
                                                                                  I: Prepend[OldIn, TI],
                                                                                  O: Prepend[OldOut, TO]):
  Aux[H, Outlet[H] :: TO, _0, Inlet[H] :: TI, _0, OldIn, OldOut, I.Out, O.Out] =
    new Can[H, Outlet[H] :: TO, _0, Inlet[H] :: TI, _0, OldIn, OldOut] {
      type NewIn = I.Out
      type NewOut = O.Out
    }
}
