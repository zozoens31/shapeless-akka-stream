package generics

import shapeless.{:+:, CNil, Coproduct, Generic, Witness}

/**
  * Created by cyrille on 03/01/2017.
  */
trait Values[T] {
  def values: List[T]
}

object Values {
  def apply[T](implicit v: Values[T]): Set[T] = v.values.toSet

  implicit def values[T, Repr <: Coproduct]
  (implicit gen: Generic.Aux[T, Repr], v: Aux[T, Repr]): Values[T] =
    new Values[T] {
      def values = v.values
    }

  trait Aux[T, Repr] {
    def values: List[T]
  }

  object Aux {
    implicit def cnilAux[A]: Aux[A, CNil] =
      new Aux[A, CNil] {
        def values = Nil
      }

    implicit def cconsAux[T, L <: T, R <: Coproduct]
    (implicit l: Witness.Aux[L], r: Aux[T, R]): Aux[T, L :+: R] =
      new Aux[T, L :+: R] {
        def values = l.value :: r.values
      }
  }

}