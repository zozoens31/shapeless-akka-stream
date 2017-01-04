package generics

case class Vector2D[T](x: T, y: T) {
  def +(that: Vector2D[T]): Vector2D[T] = ???
}

case class Vector3D[T](x: T, y: T, z: T) {
  def +(that: Vector3D[T]): Vector3D[T] = ???
}

