package manuel

object Mnl {
  val usage =
    """usage: [--version][-h|--help]""".stripMargin

  def ok[T](msg: String) = {
    println(msg)
    0
  }

  def err[T](msg: String = "") = {
    if (!msg.isEmpty) System.err.println(msg)
    1
  }

  def apply(args: Array[String]): Int =
   args.toList match {
     case List("--version") =>
       ok("0.1.0")
     case List("-h" | "--help") =>
       ok(usage)
     case _ =>
       err(usage)
   }
}

object Main {
  def main(args: Array[String]) {
    System.exit(Mnl(args))
  }
}

class Script extends xsbti.AppMain {
  def run(config: xsbti.AppConfiguration) =
    new Exit(Mnl(config.arguments))
}

class Exit(val code: Int) extends xsbti.Exit
