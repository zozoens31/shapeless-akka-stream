import generics.{LoginInfo, Name, Player}
import shapeless.PolyDefns.->
import shapeless.{Poly1, everywhere}

val u: Player = Player(
  Name("Alphonse"),
  Name("daudet"),
  Name("zogzog"),
  LoginInfo(
    Name("zogzog")
  ),
  5,
  List(Name("bottle of rhum"))
)

object capitalizeName extends ->((n: Name) => Name(n.value.capitalize))

object myPoly extends Poly1 {
  implicit def caseName = at[Name](n => Name(n.value + "!"))
  implicit def caseInt = at[Int](_ + 1)
}
//myPoly(u)