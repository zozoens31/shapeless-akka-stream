package akka.stream

import shapeless.{::, AllAre, HNil}

/**
  * Created by cyrille on 19/11/2016.
  */
package object scaladsl {
  import scala.language.implicitConversions

  implicit def source2Hlist[O, M](source: Source[O, M]): HListGraph[HNil, ::[Outlet[O], HNil], M] =
    HListGraph.fromGraph(GraphDSL.create(source){_ => s => HListShape(HNil: HNil, s.out :: HNil)(AllAre.hnil, AllAre.hCons)})

  implicit def sink2Hlist[I, M](sink: Sink[I, M]): HListGraph[::[Inlet[I], HNil], HNil, M] =
    HListGraph.fromGraph(GraphDSL.create(sink){_ => s => HListShape(s.in :: HNil, HNil: HNil)(AllAre.hCons, AllAre.hnil)})

  implicit def fanout22Hlist[I, O1, O2, M](fanout: Graph[FanOutShape2[I, O1, O2], M]): HListGraph[::[Inlet[I], HNil], ::[Outlet[O1], ::[Outlet[O2], HNil]], M] =
    HListGraph.fromGraph(GraphDSL.create(fanout){_ => s => HListShape(s.in :: HNil, s.out0 :: s.out1 :: HNil)})

  implicit def hlist2flow[I, O, M](flow: Graph[HListShape[Inlet[I] :: HNil, Outlet[O] :: HNil], M]): Flow[I, O, M] =
    Flow.fromGraph(GraphDSL.create(flow){_ => f => FlowShape(f.ins.head, f.outs.head)})


}
