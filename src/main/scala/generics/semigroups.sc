import generics.{Vector2D, Vector3D}

val v1: Vector3D[Int] = Vector3D(12, 13, 14)
val v2: Vector3D[Int] = Vector3D(1, 2, 3)

import generics.syntax._

v1 |+| v2

val v: Vector2D[Vector3D[Int]] = Vector2D(v1, v2)

v |* 2