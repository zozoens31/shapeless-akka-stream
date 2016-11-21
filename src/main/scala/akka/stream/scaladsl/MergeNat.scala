package akka.stream.scaladsl

import akka.stream.{Attributes, Inlet, Outlet}
import akka.stream.stage.{GraphStage, GraphStageLogic}
import shapeless.{::, AllAre, BuildAllAre, HList, HNil, Nat}
import shapeless.ops.nat.ToInt

/**
  * Created by cyrille on 21/11/2016.
  */
class MergeNat[T, N <: Nat, O <: HList](eagerComplete: Boolean = false)(implicit val n: ToInt[N], val baa: BuildAllAre.Aux[N, T, Inlet, O]) extends GraphStage[HListShape[O, Outlet[T] :: HNil]] {
  private val merge = Merge[T](n(), eagerComplete)
  @scala.throws[Exception](classOf[Exception])
  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = merge.createLogic(inheritedAttributes)

  override def shape: HListShape[baa.Out, Outlet[T] :: HNil] = HListShape(baa.allAre.fromSeq(merge.in), merge.out :: HNil)(baa.allAre, implicitly[(Outlet[T] :: HNil) AllAre Outlet])
}

object MergeNat {
  def apply[T, N <: Nat](eagerComplete: Boolean = false)(implicit n: ToInt[N], baa: BuildAllAre[N, T, Inlet]) = new MergeNat[T, N, baa.Out](eagerComplete)(n, baa)
}
