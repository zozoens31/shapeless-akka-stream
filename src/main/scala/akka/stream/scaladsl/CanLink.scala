package akka.stream
package scaladsl

import akka.stream.scaladsl.GraphDSL.Builder
import shapeless.{::, AllAre, HList, HNil, MyPrepend, Nat, Succ, _0}

import scala.annotation.implicitNotFound

/**
  * Created by cyrille on 20/11/2016.
  */
@implicitNotFound("You cannot link the following shapes:\n  left input ${I0}\n  left output ${O0}\n  left output index ${On}\n  right input index ${In}\n  right input ${I1}\n  right output ${O1}")
trait CanLink[O0 <: HList, On <: Nat, I1 <: HList, In <: Nat, I0 <: HList, O1 <: HList] {
  type NewIn <: HList
  type NewOut <: HList

  def allAreIn: AllAre[NewIn, Inlet]

  def allAreOut: AllAre[NewOut, Outlet]

  def apply[M](outs: O0, ins: I1)(implicit builder: GraphDSL.Builder[M]): Unit

  def ins(oldIns: I0, ins: I1): NewIn

  def outs(outs: O0, oldOuts: O1): NewOut
}

object CanLink {
  type Aux[O0 <: HList, On <: Nat, I1 <: HList, In <: Nat, I0 <: HList, O1 <: HList, NewI <: HList, NewO <: HList] = CanLink[O0, On, I1, In, I0, O1] {
    type NewIn = NewI
    type NewOut = NewO
  }

  implicit def head[H0,
  H1 >: H0,
  I0 <: HList,
  TI <: HList,
  O1 <: HList,
  TO <: HList
  ](implicit
    I: MyPrepend[I0, TI],
    AI0: AllAre[I0, Inlet],
    AI1: AllAre[TI, Inlet],
    O: MyPrepend[O1, TO],
    AO0: AllAre[O1, Outlet],
    AO1: AllAre[TO, Outlet]
   ):
  Aux[Outlet[H0] :: TO, _0, Inlet[H1] :: TI, _0, I0, O1, I.Out, O.Out] =
    new CanLink[Outlet[H0] :: TO, _0, Inlet[H1] :: TI, _0, I0, O1] {
      type NewIn = I.Out
      type NewOut = O.Out
      val allAreIn = AllAre.prepend[Inlet, I0, TI]
      val allAreOut = AllAre.prepend[Outlet, O1, TO]

      def apply[M](outs: Outlet[H0] :: TO, ins: Inlet[H1] :: TI)(implicit builder: GraphDSL.Builder[M]) = {
        import GraphDSL.Implicits._
        outs.head ~> ins.head
      }

      def ins(oldIns: I0, ins: Inlet[H1] :: TI) = I(oldIns, ins.tail)

      def outs(outs: Outlet[H0] :: TO, oldOuts: O1) = O(oldOuts, outs.tail)
    }

  implicit def unstackLeft[H0,
  H1,
  K,
  TO <: HList,
  N <: Nat,
  TI <: HList,
  In <: Nat,
  I0 <: HList,
  O1 <: HList](implicit C: CanLink[TO, N, TI, In, I0, O1]): Aux[Outlet[K] :: TO, Succ[N], TI, In, I0, O1, C.NewIn, Outlet[K] :: C.NewOut] =
    new CanLink[Outlet[K] :: TO, Succ[N], TI, In, I0, O1] {
      type NewIn = C.NewIn
      type NewOut = Outlet[K] :: C.NewOut
      val allAreIn = C.allAreIn
      val allAreOut = AllAre.hCons[Outlet, K, C.NewOut](C.allAreOut)

      def apply[M](outs: ::[Outlet[K], TO], ins: TI)(implicit builder: GraphDSL.Builder[M]) = C(outs.tail, ins)

      def ins(oldIns: I0, ins: TI) = C.ins(oldIns, ins)

      def outs(outs: ::[Outlet[K], TO], oldOuts: O1) = outs.head :: C.outs(outs.tail, oldOuts)
    }

  implicit def unstackRight[K, TO <: HList, On <: Nat, TI <: HList, N <: Nat, OldIn <: HList, OldOut <: HList](implicit C: CanLink[TO, On, TI, N, OldIn, OldOut]): Aux[TO, N, Inlet[K] :: TI, Succ[N], OldIn, OldOut, Inlet[K] :: C.NewIn, C.NewOut] =
    new CanLink[TO, N, Inlet[K] :: TI, Succ[N], OldIn, OldOut] {
      type NewIn = Inlet[K] :: C.NewIn
      type NewOut = C.NewOut
      val allAreIn = AllAre.hCons[Inlet, K, C.NewIn](C.allAreIn)
      val allAreOut = C.allAreOut

      def apply[M](outs: TO, ins: ::[Inlet[K], TI])(implicit builder: GraphDSL.Builder[M]) = C.apply(outs, ins.tail)

      def ins(oldIns: OldIn, ins: ::[Inlet[K], TI]) = ins.head :: C.ins(oldIns, ins.tail)

      def outs(outs: TO, oldOuts: OldOut) = C.outs(outs, oldOuts)
    }
}

trait CanLinkAll[O <: HList, I <: HList] {
  def linkAll[M](outs: O, ins: I)(implicit b: GraphDSL.Builder[M]): Unit
}

object CanLinkAll {
  implicit val hnil = new CanLinkAll[HNil, HNil] {
    override def linkAll[M](outs: HNil, ins: HNil)(implicit b: Builder[M]) = ()
  }

  implicit def hcons[H0, H1 >: H0, O <: HList, I <: HList](implicit tail: O CanLinkAll I) = new CanLinkAll[Outlet[H0] :: O, Inlet[H1] :: I] {
    override def linkAll[M](outs: ::[Outlet[H0], O], ins: ::[Inlet[H1], I])(implicit b: Builder[M]) = {
      import GraphDSL.Implicits._
      outs.head ~> ins.head
      tail.linkAll(outs.tail, ins.tail)
    }
  }
}