sealed trait Player {
  def getSign(): String
}
class PlayerA extends Player {
  override def getSign(): String = "X"
}

class PlayerB extends Player {
  override def getSign(): String = "O"
}

case class Cell(maybePlayer: Option[Player]) {
  override def toString: String = maybePlayer match {
    case Some(value) => value.getSign()
    case None => " "
  }
}