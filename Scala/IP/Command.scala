trait Command[T] {
def execute(target: T): Option[Change]
}

object makeTransaction {

  def apply[T](commands: List[Command[T]]) = {
    var com = commands(0)
    var i = 1
    while (i < commands.length){
      com = new AndThenCommand(com, commands(i))
      i += 1
    }
    com
  }

}
