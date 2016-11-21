package akka.stream.scaladsl

import akka.stream.{Attributes, Inlet, Outlet}
import akka.stream.stage.{GraphStage, GraphStageLogic}
import shapeless.{::, AllAre, BuildUniform, HList, HNil, Nat}
import shapeless.ops.nat.ToInt

/**
  * Created by cyrille on 21/11/2016.
  */
class MergeNat[T, N <: Nat, O <: HList](eagerComplete: Boolean = false)(implicit val n: ToInt[N], val U: BuildUniform.Aux[N, T, Inlet, O]) extends GraphStage[HListShape[O, Outlet[T] :: HNil]] {
  private val merge = Merge[T](n(), eagerComplete)
  @scala.throws[Exception](classOf[Exception])
  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = merge.createLogic(inheritedAttributes)

  override def shape: HListShape[U.Out, Outlet[T] :: HNil] = HListShape(U.allAre.fromSeq(merge.in), merge.out :: HNil)(U.allAre, implicitly[(Outlet[T] :: HNil) AllAre Outlet])
}

object MergeNat {
  def apply[T, N <: Nat](eagerComplete: Boolean = false)(implicit n: ToInt[N], U: BuildUniform[N, T, Inlet]) = new MergeNat[T, N, U.Out](eagerComplete)(n, U)
}
