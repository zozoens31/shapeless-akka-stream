package generics

sealed trait GameCharacter

case class Name(value: String) {
  override def toString = value
}

case class NPC(name: Name, level: Int, weapon: String) extends GameCharacter

case class LoginInfo(login: Name)

case class Player(firstName: Name, lastName: Name, inGameName: Name, login: LoginInfo, level: Int,  inventory: List[Name]) extends GameCharacter