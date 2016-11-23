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

object AllAre extends AllAreBuildable with AllAreLowerImplicits {
  implicit def hnil[F[_]]: AllAre[HNil, F] = new AllAre[HNil, F] {
    override def toSeq(l: HNil): Seq[F[_]] = Seq()

    override def allApply(l: HNil)(f: ~>[F, F]): HNil = l

    override def fromSeq(s: Seq[F[_]]): HNil = {
      require(s.isEmpty, s"too many elements in Seq [${s.mkString(", ")}]")
      HNil
    }
  }

  implicit def hConsO[H, T <: HList](implicit tail: T AllAre Outlet): AllAre[::[Outlet[H], T], Outlet] = new AllAre[Outlet[H] :: T, Outlet] {
    type F[X] = Outlet[X]
    override def toSeq(l: F[H] :: T): Seq[F[_]] = l.head +: tail.toSeq(l.tail)

    override def allApply(l: F[H] :: T)(f: F ~> F): F[H] :: T = f(l.head) :: tail.allApply(l.tail)(f)

    override def fromSeq(s: Seq[F[_]]): F[H] :: T = {
      require(s.nonEmpty, s"not enough elements in Seq")
      require(s.head.isInstanceOf[F[H]], s"invalid type of head element [${s.head}]")
      s.head.as[H] :: tail.fromSeq(s.tail)
    }
  }
  implicit def hConsI[H, T <: HList](implicit tail: T AllAre Inlet): AllAre[::[Inlet[H], T], Inlet] = new AllAre[Inlet[H] :: T, Inlet] {
    type F[X] = Inlet[X]
    override def toSeq(l: F[H] :: T): Seq[F[_]] = l.head +: tail.toSeq(l.tail)

    override def allApply(l: F[H] :: T)(f: F ~> F): F[H] :: T = f(l.head) :: tail.allApply(l.tail)(f)

    override def fromSeq(s: Seq[F[_]]): F[H] :: T = {
      require(s.nonEmpty, s"not enough elements in Seq")
      require(s.head.isInstanceOf[F[H]], s"invalid type of head element [${s.head}]")
      s.head.as[H] :: tail.fromSeq(s.tail)
    }
  }
}

trait AllAreLowerImplicits {
  implicit def hCons[F[_], H, T <: HList](implicit tail: T AllAre F): AllAre[::[F[H], T], F] = new AllAre[F[H] :: T, F] {
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
      val LL = L.asInstanceOf[MyPrepend.Aux[L0, L1, L]]    //workaround for SI-9247 https://issues.scala-lang.org/browse/SI-9247

      override def toSeq(l: L): Seq[F[_]] = l match {
        case LL(l0, l1) => A0.toSeq(l0) ++ A1.toSeq(l1)
      }

      override def allApply(l: L)(f: ~>[F, F]): L = l match {
        case LL(l0, l1) => LL(A0.allApply(l0)(f), A1.allApply(l1)(f))
      }

      override def fromSeq(s: Seq[F[_]]): L = {
        val l0 = L.leftSize
        L(A0.fromSeq(s.take(l0)), A1.fromSeq(s.drop(l0)))
      }
    }
}

trait BuildUniform[N <: Nat, K, F[_]] {
  type Out <: HList
  def allAre: Out AllAre F
}

object BuildUniform {
  type Aux[N <: Nat, K, F[_], O <: HList] = BuildUniform[N, K, F] {
    type Out = O
  }

  implicit def zero[F[_], K]: Aux[_0, K, F, HNil] = new BuildUniform[_0, K, F] {
    type Out = HNil
    def allAre = AllAre.hnil[F]
  }

  implicit def succ[N <: Nat, K, F[_]](implicit U: BuildUniform[N, K, F]): Aux[Succ[N], K, F, F[K] :: U.Out] =
    new BuildUniform[Succ[N], K, F] {
      type Out = F[K] :: U.Out
      def allAre = AllAre.hCons[F, K, U.Out](U.allAre)
    }
}

