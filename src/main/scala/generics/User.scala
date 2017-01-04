package generics

sealed trait Sex

object Sex {
  case object Male extends Sex
  case object Female extends Sex
  case object Undefined extends Sex

  def values: Set[Sex] = Values[Sex]
}

case class Address(number: Int,
                   street: String,
                   city: String,
                   zipCode: String,
                   country: String
                  )

case class User(name: String,
                age: Int,
                sex: Sex,
                addresses: List[Address]
               )

object User {
  val format = implicitly[Format[User]]
}

