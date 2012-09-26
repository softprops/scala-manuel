package manuel

import java.io.File

import java.io._

object Main {
  def main(a: Array[String]) {
    val path = a match {
      case Array(path) => path
      case _ => "man/mnl.1.ronn"
    }
   
    val f = new File(path)
    val man = Manual(f)

    new Roff.Writer(man).write()
    println("wrote roff %s" format man.basename(""))
    new Html.Writer(man).write()
    println("write html %s" format man.basename("html"))
   
    val b = new ProcessBuilder(
      "/bin/sh", "-c", 
      "groff -Wall -mtty-char -mandoc -Tascii < %s".format(
        new File(man.basename(""))
      ))
     b.redirectErrorStream()
     val p = b.start()
     p.waitFor()
  }
}
