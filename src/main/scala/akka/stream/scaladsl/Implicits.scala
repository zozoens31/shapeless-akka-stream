package akka.stream
package scaladsl

import shapeless.{::, AllAre, HList, HNil}

/**
  * Created by cyrille on 21/11/2016.
  */
object Implicits {
  import scala.language.implicitConversions

  implicit def source2Hlist[O, M](source: Source[O, M]): HListGraph[HNil, ::[Outlet[O], HNil], M] =
    HListGraph.fromGraph(GraphDSL.create(source){_ => s => HListShape(HNil: HNil, s.out :: HNil)(AllAre.hnil, AllAre.hCons)})

  implicit def sink2Hlist[I, M](sink: Sink[I, M]): HListGraph[::[Inlet[I], HNil], HNil, M] =
    HListGraph.fromGraph(GraphDSL.create(sink){_ => s => HListShape(s.in :: HNil, HNil: HNil)(AllAre.hCons, AllAre.hnil)})

  implicit def flow2Hlist[I, O, M](flow: Flow[I, O, M]): HListGraph[::[Inlet[I], HNil], Outlet[O] :: HNil, M] =
    HListGraph.fromGraph(GraphDSL.create(flow){_ => s => HListShape(s.in :: HNil, s.out :: HNil)(AllAre.hCons, AllAre.hCons)})

  implicit def fanout22Hlist[I, O1, O2, M](fanout: Graph[FanOutShape2[I, O1, O2], M]): HListGraph[::[Inlet[I], HNil], ::[Outlet[O1], ::[Outlet[O2], HNil]], M] =
    HListGraph.fromGraph(GraphDSL.create(fanout){_ => s => HListShape(s.in :: HNil, s.out0 :: s.out1 :: HNil)})

  implicit def hlist2flow[I, O, M](flow: Graph[HListShape[Inlet[I] :: HNil, Outlet[O] :: HNil], M]): Flow[I, O, M] =
    Flow.fromGraph(GraphDSL.create(flow){_ => f => FlowShape(f.ins.head, f.outs.head)})

  implicit def graphToHlist[I <: HList, O <: HList, M](graph: Graph[HListShape[I, O], M]): HListGraph[I, O, M] = HListGraph.fromGraph(graph)
}
