package akka.stream
package scaladsl

import akka.stream.scaladsl.GraphDSL.Builder
import impl.StreamLayout.Module
import shapeless.ops.hlist.{Length, Prepend, Split}
import shapeless.ops.nat.ToInt
import shapeless.{::, HList, HNil, Nat, Succ, _0}

import scala.annotation.unchecked.uncheckedVariance

/**
  * Created by cyrille on 18/11/2016.
  */
final class HListGraph[-I0 <: HList, +O0 <: HList, +M0](override val module: Module) extends Graph[HListShape[I0, O0], M0] { self =>
  override val shape = module.shape.asInstanceOf[HListShape[I0, O0]]

  override def withAttributes(attr: Attributes) =
    new HListGraph[I0, O0, M0](module.withAttributes(attr))

  override def toString: String = s"HListGraph($shape, $module)"

  def stackMat[
  I1 <: HList,
  O1 <: HList,
  I <: HList,
  O <: HList,
  NI <: Nat,
  NO <: Nat,
  II0 <: HList,
  II1 <: HList,
  OI0 <: HList,
  OI1 <: HList,
  M1,
  M2
  ](that: Graph[HListShape[I1, O1], M1])(combine: (M0, M1) => M2)(implicit
                                                                  I: Prepend.Aux[I0@uncheckedVariance, I1, I],
                                                                  II: Prepend[II0, II1],
                                                                  NI: Length.Aux[I0@uncheckedVariance, NI],
                                                                  nI: ToInt[NI],
                                                                  IS: Split.Aux[I, NI, I0@uncheckedVariance, I1],
                                                                  AI0: AllAre.Aux[I0@uncheckedVariance, Inlet, II0],
                                                                  AI1: AllAre.Aux[I1, Inlet, II1],
                                                                  O: Prepend.Aux[O0@uncheckedVariance, O1, O],
                                                                  OI: Prepend[OI0, OI1],
                                                                  NO: Length.Aux[O0@uncheckedVariance, NO],
                                                                  nO: ToInt[NO],
                                                                  OS: Split.Aux[O, NO, O0@uncheckedVariance, O1],
                                                                  AO0: AllAre.Aux[O0@uncheckedVariance, Outlet, OI0],
                                                                  AO1: AllAre.Aux[O1, Outlet, OI1]
  ) =
    HListGraph.fromGraph(GraphDSL.create(this, that)(combine) { _ =>
      (zero, one) =>
        HListShape(I(zero.ins, one.ins), O(zero.outs, one.outs))(AllAre.prepend[Inlet, I0, I1, I, II0, II1, NI], AllAre.prepend[Outlet, O0, O1, O, OI0, OI1, NO])
    })

  def stack[
  I1 <: HList,
  O1 <: HList,
  I <: HList,
  O <: HList,
  NI <: Nat,
  NO <: Nat,
  II0 <: HList,
  II1 <: HList,
  OI0 <: HList,
  OI1 <: HList,
  M1](that: Graph[HListShape[I1, O1], M1])(implicit
                                           I: Prepend.Aux[I0@uncheckedVariance, I1, I],
                                           II: Prepend[II0, II1],
                                           NI: Length.Aux[I0@uncheckedVariance, NI],
                                           nI: ToInt[NI],
                                           IS: Split.Aux[I, NI, I0@uncheckedVariance, I1],
                                           AI0: AllAre.Aux[I0@uncheckedVariance, Inlet, II0],
                                           AI1: AllAre.Aux[I1, Inlet, II1],
                                           O: Prepend.Aux[O0@uncheckedVariance, O1, O],
                                           OI: Prepend[OI0, OI1],
                                           NO: Length.Aux[O0@uncheckedVariance, NO],
                                           nO: ToInt[NO],
                                           OS: Split.Aux[O, NO, O0@uncheckedVariance, O1],
                                           AO0: AllAre.Aux[O0@uncheckedVariance, Outlet, OI0],
                                           AO1: AllAre.Aux[O1, Outlet, OI1]
  ) = stackMat[I1, O1, I, O, NI, NO, II0, II1, OI0, OI1, M1, M0](that)(Keep.left)

  def toMat[O0n <: Nat, I1n <: Nat] = new ToMat[O0n, I1n] {
    def apply[H, I1 <: HList, O1 <: HList, M1, M2](that: Graph[HListShape[I1, O1], M1])(combine: (M0, M1) => M2)(implicit link: CanLink[H, O0@uncheckedVariance, O0n, I1, I1n, I0@uncheckedVariance, O1]): HListGraph[link.NewIn, link.NewOut, M2] =
      HListGraph.fromGraph(GraphDSL.create(self, that)(combine) { implicit builder =>
        (left, right) =>
          link(left.outs, right.ins)
          HListShape(link.ins(left.ins, right.ins), link.outs(left.outs, right.outs))(link.allAreIn, link.allAreOut)
      })
  }

  trait ToMat[O0n <: Nat, I1n <: Nat] {
    def apply[H, I1 <: HList, O1 <: HList, M1, M2](that: Graph[HListShape[I1, O1], M1])(combine: (M0, M1) => M2)(implicit link: CanLink[H, O0@uncheckedVariance, O0n, I1, I1n, I0@uncheckedVariance, O1]): HListGraph[link.NewIn, link.NewOut, M2]
  }

  def to[O0n <: Nat, I1n <: Nat] = new To[O0n, I1n] {
    def apply[H, I1 <: HList, O1 <: HList, M1](that: Graph[HListShape[I1, O1], M1])(implicit link: CanLink[H, O0@uncheckedVariance, O0n, I1, I1n, I0@uncheckedVariance, O1]): HListGraph[link.NewIn, link.NewOut, M0] =
    toMat[O0n, I1n](that)(Keep.left)
  }

  trait To[O0n <: Nat, I1n <: Nat] {
    def apply[H, I1 <: HList, O1 <: HList, M1](that: Graph[HListShape[I1, O1], M1])(implicit link: CanLink[H, O0@uncheckedVariance, O0n, I1, I1n, I0@uncheckedVariance, O1]): HListGraph[link.NewIn, link.NewOut, M0]
  }

  trait To_:[O1n <: Nat, I0n <: Nat] {
    def apply[H, I1 <: HList, O1 <: HList, M1](that: Graph[HListShape[I1, O1], M1])(implicit link: CanLink[H, O1, O1n, I0@uncheckedVariance, I0n, I1, O0@uncheckedVariance]): HListGraph[link.NewIn, link.NewOut, M1]
  }
  def to_:[O1n <: Nat, I0n <: Nat] = new To_:[O1n, I0n] {
    def apply[H, I1 <: HList, O1 <: HList, M1](that: Graph[HListShape[I1, O1], M1])(implicit link: CanLink[H, O1, O1n, I0@uncheckedVariance, I0n, I1, O0@uncheckedVariance]): HListGraph[link.NewIn, link.NewOut, M1] =
      HListGraph.fromGraph(that).toMat(self)(Keep.left)
  }

  def run(implicit can: CanRun[I0 @uncheckedVariance, O0 @uncheckedVariance], mat: Materializer): M0 = can.run(this)
}

object HListGraph {
  def fromGraph[I <: HList, O <: HList, M](g: Graph[HListShape[I, O], M]): HListGraph[I, O, M] = g match {
    case s: HListGraph[I, O, M] ⇒ s
    case other ⇒ new HListGraph[I, O, M](other.module)
  }

}

trait CanRun[I <: HList, O <: HList] {
  def run[M](graph: HListGraph[I, O, M])(implicit materializer: Materializer): M
}

object CanRun {
  implicit val hnil = new CanRun[HNil, HNil] {
    override def run[M](graph: HListGraph[HNil, HNil, M])(implicit materializer: Materializer): M =
      RunnableGraph.fromGraph(GraphDSL.create(graph)(_ => _ => ClosedShape)).run()
  }
}
trait CanLink[H, O <: HList, On <: Nat, I <: HList, In <: Nat, OldIn <: HList, OldOut <: HList] {
  type NewIn <: HList
  type NewOut <: HList

  def allAreIn: AllAre[NewIn, Inlet]

  def allAreOut: AllAre[NewOut, Outlet]

  def apply[M](outs: O, ins: I)(implicit builder: Builder[M]): Unit

  def ins(oldIns: OldIn, ins: I): NewIn

  def outs(outs: O, oldOuts: OldOut): NewOut
}

object CanLink {
  type Aux[H, O <: HList, On <: Nat, I <: HList, In <: Nat, OldIn <: HList, OldOut <: HList, NewI <: HList, NewO <: HList] = CanLink[H, O, On, I, In, OldIn, OldOut] {
    type NewIn = NewI
    type NewOut = NewO
  }

  implicit def head[H,
  OldIn <: HList,
  TI <: HList,
  OldOut <: HList,
  TO <: HList,
  I <: HList,
  O <: HList,
  NI <: Nat,
  NO <: Nat,
  II0 <: HList,
  II1 <: HList,
  OI0 <: HList,
  OI1 <: HList,
  M1](implicit
      I: Prepend.Aux[OldIn, TI, I],
      II: Prepend[II0, II1],
      NI: Length.Aux[OldIn, NI],
      nI: ToInt[NI],
      IS: Split.Aux[I, NI, OldIn, TI],
      AI0: AllAre.Aux[OldIn, Inlet, II0],
      AI1: AllAre.Aux[TI, Inlet, II1],
      O: Prepend.Aux[OldOut, TO, O],
      OI: Prepend[OI0, OI1],
      NO: Length.Aux[OldOut, NO],
      nO: ToInt[NO],
      OS: Split.Aux[O, NO, OldOut, TO],
      AO0: AllAre.Aux[OldOut, Outlet, OI0],
      AO1: AllAre.Aux[TO, Outlet, OI1]
     ):
  Aux[H, Outlet[H] :: TO, _0, Inlet[H] :: TI, _0, OldIn, OldOut, I.Out, O.Out] =
    new CanLink[H, Outlet[H] :: TO, _0, Inlet[H] :: TI, _0, OldIn, OldOut] {
      type NewIn = I.Out
      type NewOut = O.Out
      val allAreIn = AllAre.prepend[Inlet, OldIn, TI, I, II0, II1, NI]
      val allAreOut = AllAre.prepend[Outlet, OldOut, TO, O, OI0, OI1, NO]

      def apply[M](outs: Outlet[H] :: TO, ins: Inlet[H] :: TI)(implicit builder: Builder[M]) = {
        import GraphDSL.Implicits._
        outs.head ~> ins.head
      }

      def ins(oldIns: OldIn, ins: ::[Inlet[H], TI]) = I(oldIns, ins.tail)

      def outs(outs: ::[Outlet[H], TO], oldOuts: OldOut) = O(oldOuts, outs.tail)
    }

  implicit def unstackLeft[H, K, TO <: HList, N <: Nat, TI <: HList, In <: Nat, OldIn <: HList, OldOut <: HList](implicit C: CanLink[H, TO, N, TI, In, OldIn, OldOut]): Aux[H, Outlet[K] :: TO, Succ[N], TI, In, OldIn, OldOut, C.NewIn, Outlet[K] :: C.NewOut] =
    new CanLink[H, Outlet[K] :: TO, Succ[N], TI, In, OldIn, OldOut] {
      type NewIn = C.NewIn
      type NewOut = Outlet[K] :: C.NewOut
      val allAreIn = C.allAreIn
      val allAreOut = AllAre.hCons[Outlet, K, C.NewOut](C.allAreOut)

      def apply[M](outs: ::[Outlet[K], TO], ins: TI)(implicit builder: Builder[M]) = C(outs.tail, ins)

      def ins(oldIns: OldIn, ins: TI) = C.ins(oldIns, ins)

      def outs(outs: ::[Outlet[K], TO], oldOuts: OldOut) = outs.head :: C.outs(outs.tail, oldOuts)
    }

  implicit def unstackRight[H, K, TO <: HList, On <: Nat, TI <: HList, N <: Nat, OldIn <: HList, OldOut <: HList](implicit C: CanLink[H, TO, On, TI, N, OldIn, OldOut]): Aux[H, TO, N, Inlet[K] :: TI, Succ[N], OldIn, OldOut, Inlet[K] :: C.NewIn, C.NewOut] =
    new CanLink[H, TO, N, Inlet[K] :: TI, Succ[N], OldIn, OldOut] {
      type NewIn = Inlet[K] :: C.NewIn
      type NewOut = C.NewOut
      val allAreIn = AllAre.hCons[Inlet, K, C.NewIn](C.allAreIn)
      val allAreOut = C.allAreOut

      def apply[M](outs: TO, ins: ::[Inlet[K], TI])(implicit builder: Builder[M]) = C.apply(outs, ins.tail)

      def ins(oldIns: OldIn, ins: ::[Inlet[K], TI]) = ins.head :: C.ins(oldIns, ins.tail)

      def outs(outs: TO, oldOuts: OldOut) = C.outs(outs, oldOuts)
    }
}
