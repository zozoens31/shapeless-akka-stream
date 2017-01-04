package generics

import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, ProductTypeClass, ProductTypeClassCompanion, TypeClass, TypeClassCompanion}

trait Semigroup[T] {
  def append(a: T, b: T): T
  def times(a: T, n: Int): T = {
    require(n > 0)
    if (n == 1) a else append(a, times(a, n - 1))
  }
}

object syntax {
  implicit class SemigroupOps[T](a: T)(implicit T: Semigroup[T]) {
    def |+|(b: T) = T.append(a, b)
    def |*(n: Int) = T.times(a, n)
  }
}

object Semigroup extends ProductTypeClassCompanion[Semigroup] {
  def fromAppend[T](app: (T, T) => T) = new Semigroup[T] {
    override def append(a: T, b: T): T = app(a, b)
  }
  implicit val booleanSG = Semigroup.fromAppend[Boolean](_ || _)

  implicit def numericSG[T](implicit T: Numeric[T]) = {
    import T.mkNumericOps
    Semigroup.fromAppend[T](_ + _)
  }

  implicit val stringSG = Semigroup.fromAppend[String](_ + _)

  object typeClass extends ProductTypeClass[Semigroup] {
    override def product[H, T <: HList](ch: Semigroup[H], ct: Semigroup[T]): Semigroup[H :: T] =
      Semigroup.fromAppend[H :: T]((l1, l2) => ch.append(l1.head, l2.head) :: ct.append(l1.tail, l2.tail))

    override def emptyProduct: Semigroup[HNil] =
      Semigroup.fromAppend[HNil]((_, _) => HNil)

    override def project[F, G](instance: => Semigroup[G], to: (F) => G, from: (G) => F): Semigroup[F] =
      Semigroup.fromAppend[F]((f1, f2) => from(instance.append(to(f1), to(f2))))

    def coproduct[L, R <: Coproduct](cl: =>Semigroup[L], cr: =>Semigroup[R]): Semigroup[:+:[L, R]] =
      Semigroup.fromAppend[L :+: R]{
        case (Inl(l1), Inl(l2)) => Inl(cl.append(l1, l2))
        case (Inr(r1), Inr(r2)) => Inr(cr.append(r1, r2))
        case (Inr(r1), _) => Inr(r1)
        case (_, Inr(r2)) => Inr(r2)
      }

    def emptyCoproduct: Semigroup[CNil] =
      Semigroup.fromAppend[CNil]((a, _) => a)
  }
}
