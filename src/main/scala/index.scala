package manuel

import io.Source
import java.io.File

/** The index stores link information about manuals and section */
case class Index(base: File, mans: Seq[Manual] = Nil) {

  case class Ref(name: String, loc: String) {
    def isRonn = loc.matches("""\.ronn?$""")
    def isRelative = false
    def isManual = name.matches("""\([0-9]\w*\)$""")
    def url = loc + ".html"
    def path =
      new File(new File(Index.this.path.getPath).getParentFile(), loc).getPath
  }

  def include(man: Manual) = this.copy(base, mans :+ man)

  def path =
    if(base.isDirectory) new File(base, "index.txt")
    else new File(base.getParentFile(), "index.txt")

  lazy val references =
    parse
      
  def parse =
    io.Source.fromFile(base).getLines.map {
      case line =>
        val Array(name, url) = line.split("  ")
        Ref(name, url)
    }                              
}
