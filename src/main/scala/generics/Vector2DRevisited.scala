package generics

case class Vector2DRevisited[T](x: T, y: T)

object Vector2DRevisited {
  implicit def semigroup[T: Semigroup] = new Semigroup[Vector2DRevisited[T]] {
    import syntax._
    override def append(a: Vector2DRevisited[T], b: Vector2DRevisited[T]): Vector2DRevisited[T] =
      Vector2DRevisited(a.x |+| b.x, a.y |+| b.y)
  }
}

case class Vector3DRevisited[T](x: T, y: T, z: T)

object Vector3DRevisited {
  implicit def semigroup[T: Semigroup] = new Semigroup[Vector3DRevisited[T]] {
    import syntax._
    override def append(a: Vector3DRevisited[T], b: Vector3DRevisited[T]): Vector3DRevisited[T] =
      Vector3DRevisited(a.x |+| b.x, a.y |+| b.y, a.z |+| b.z)
  }
}

