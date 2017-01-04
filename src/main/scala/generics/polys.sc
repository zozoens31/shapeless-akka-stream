import generics._
import shapeless.PolyDefns.{->, ~>}
import shapeless._

object capitalizeNameAndIncInt extends Poly1 {
  implicit def caseName = at[Name](n => Name(n.value.capitalize))
  implicit def caseInt = at[Int](_ + 1)
}

object inc extends ->((i: Int) => i + 1)

List(1, 2, 3).map(capitalizeNameAndIncInt)
List(Name("hello"), Name("world")).map(capitalizeNameAndIncInt)
//List("hello", "world").map(capitalizeNameAndIncInt)
1 :: Name("hello world") :: HNil map capitalizeNameAndIncInt
//1 :: "hello world" :: HNil map capitalizeNameAndIncInt
Coproduct[IntName](3).map(capitalizeNameAndIncInt)
//Coproduct[IntNameString](3).map(capitalizeNameAndIncInt)








object dropHead extends (List ~> List) {
  override def apply[T](f: List[T]): List[T] = f.drop(1)
}

object double extends (Id ~> List) {
  override def apply[T](t: Id[T]): List[T] = List(t, t)
}

type IntName = Int :+: Name :+: CNil
type IntNameString = Int :+: Name :+: String :+: CNil


double(5)
List(1, 2, 3).flatMap(double)
dropHead(List(1, 2, 3))
//dropHead(5)
1 :: "hello world" :: HNil map double