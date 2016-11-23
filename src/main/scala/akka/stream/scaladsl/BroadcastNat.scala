package akka.stream.scaladsl

import akka.stream.stage.{GraphStage, GraphStageLogic}
import akka.stream.{Attributes, Inlet, Outlet}
import shapeless.ops.nat.ToInt
import shapeless.{::, AllAre, BuildUniform, HList, HNil, Nat}

/**
  * Created by cyrille on 21/11/2016.
  */
class BroadcastNat[T, N <: Nat, O <: HList](eagerCancel: Boolean = false)(implicit val n: ToInt[N], val U: BuildUniform.Aux[N, T, Outlet, O]) extends GraphStage[HListShape[Inlet[T] :: HNil, O]] {
  private val broadcast = Broadcast[T](n(), eagerCancel)
  @scala.throws[Exception](classOf[Exception])
  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = broadcast.createLogic(inheritedAttributes)

  override def shape: HListShape[::[Inlet[T], HNil], O] = HListShape(broadcast.in :: HNil, U.allAre.fromSeq(broadcast.out))(implicitly[(Inlet[T] :: HNil) AllAre Inlet], U.allAre)
}

object BroadcastNat {
  def apply[T, N <: Nat](eagerCancel: Boolean = false)(implicit n: ToInt[N], U: BuildUniform[N, T, Outlet]) = new BroadcastNat[T, N, U.Out](eagerCancel)(n, U)
}
