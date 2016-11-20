package shapeless

import akka.stream.{Inlet, Outlet}
import PolyDefns.~>

import scala.language.higherKinds
import scala.collection.immutable.Seq

trait AllAre[L <: HList, F[_]] {
  def toSeq(l: L): Seq[F[_]]

  def allApply(l: L)(f: F ~> F): L

  def fromSeq(s: Seq[F[_]]): L
}

object AllAre extends AllAreBuildable {
  implicit def hnil[F[_]]: AllAre[HNil, F] = new AllAre[HNil, F] {
    override def toSeq(l: HNil): Seq[F[_]] = Seq()

    override def allApply(l: HNil)(f: ~>[F, F]): HNil = l

    override def fromSeq(s: Seq[F[_]]): HNil = {
      require(s.isEmpty, s"too many elements in Seq [${s.mkString(", ")}]")
      HNil
    }
  }

  implicit def hCons[F[_], H, T <: HList](implicit tail: T AllAre F): AllAre[::[F[H], T], F] = new AllAre[F[H] :: T, F] {
    override def toSeq(l: F[H] :: T): Seq[F[_]] = l.head +: tail.toSeq(l.tail)

    override def allApply(l: F[H] :: T)(f: F ~> F): F[H] :: T = f(l.head) :: tail.allApply(l.tail)(f)

    override def fromSeq(s: Seq[F[_]]): F[H] :: T = {
      require(s.nonEmpty, s"not enough elements in Seq")
      require(s.head.isInstanceOf[F[H]], s"invalid type of head element [${s.head}]")
      s.head.asInstanceOf[F[H]] :: tail.fromSeq(s.tail)
    }
  }
  implicit def hConsO[H, T <: HList](implicit tail: T AllAre Outlet): AllAre[::[Outlet[H], T], Outlet] = new AllAre[Outlet[H] :: T, Outlet] {
    type F[X] = Outlet[X]
    override def toSeq(l: F[H] :: T): Seq[F[_]] = l.head +: tail.toSeq(l.tail)

    override def allApply(l: F[H] :: T)(f: F ~> F): F[H] :: T = f(l.head) :: tail.allApply(l.tail)(f)

    override def fromSeq(s: Seq[F[_]]): F[H] :: T = {
      require(s.nonEmpty, s"not enough elements in Seq")
      require(s.head.isInstanceOf[F[H]], s"invalid type of head element [${s.head}]")
      s.head.asInstanceOf[F[H]] :: tail.fromSeq(s.tail)
    }
  }
  implicit def hConsI[H, T <: HList](implicit tail: T AllAre Inlet): AllAre[::[Inlet[H], T], Inlet] = new AllAre[Inlet[H] :: T, Inlet] {
    type F[X] = Inlet[X]
    override def toSeq(l: F[H] :: T): Seq[F[_]] = l.head +: tail.toSeq(l.tail)

    override def allApply(l: F[H] :: T)(f: F ~> F): F[H] :: T = f(l.head) :: tail.allApply(l.tail)(f)

    override def fromSeq(s: Seq[F[_]]): F[H] :: T = {
      require(s.nonEmpty, s"not enough elements in Seq")
      require(s.head.isInstanceOf[F[H]], s"invalid type of head element [${s.head}]")
      s.head.asInstanceOf[F[H]] :: tail.fromSeq(s.tail)
    }
  }
}

trait AllAreBuildable {
  self: AllAre.type =>
  def prepend[F[_], L0 <: HList, L1 <: HList](implicit
                                              L: MyPrepend[L0, L1],
                                              A0: AllAre[L0, F],
                                              A1: AllAre[L1, F]): AllAre[L.Out, F] =
    new AllAre[L.Out, F] {
      type L = L.Out

      override def toSeq(l: L): Seq[F[_]] = {
        val (l0, l1) = L.unapply(l).get
        A0.toSeq(l0) ++ A1.toSeq(l1)
      }

      override def allApply(l: L)(f: ~>[F, F]): L = {
        val (l0, l1) = L.unapply(l).get
        L(A0.allApply(l0)(f), A1.allApply(l1)(f))
      }

      override def fromSeq(s: Seq[F[_]]): L = {
        val l0 = L.leftSize
        L(A0.fromSeq(s.take(l0)), A1.fromSeq(s.drop(l0)))
      }
    }

}

