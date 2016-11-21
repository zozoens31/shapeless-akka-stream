package akka.stream
package scaladsl

import shapeless.{HList, HNil}

import scala.annotation.implicitNotFound

/**
  * Created by cyrille on 20/11/2016.
  */
@implicitNotFound("You cannot run a graph with nonEmpty inputs ${I} or outputs ${O}")
trait CanRun[I <: HList, O <: HList] {
  def run[M](graph: HListGraph[I, O, M])(implicit materializer: Materializer): M
}

object CanRun {
  implicit val hnil = new CanRun[HNil, HNil] {
    override def run[M](graph: HListGraph[HNil, HNil, M])(implicit materializer: Materializer): M =
      RunnableGraph.fromGraph(GraphDSL.create(graph)(_ => _ => ClosedShape)).run()
  }
}
