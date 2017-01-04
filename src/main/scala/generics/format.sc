import generics._
import play.api.libs.json.Json

implicit val f: Format[User] = User.format
val user = User(
  "name",
  29,
  Sex.Male,
  List(
    Address(
      12,
      "street",
      "city",
      "zipcode",
      "country"
    )
  )
)

val json = Json.toJson(user)

assert(json.validate[User].get == user)