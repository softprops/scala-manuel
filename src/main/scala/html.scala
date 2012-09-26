package manuel

import com.tristanhunt.knockoff._
import com.github.mustachejava.DefaultMustacheFactory
import java.io.FileWriter

object Html {
  case class Page(man: Manual) {
    def sections = man.toc
    def title = man.title
    def name = man.name
    def section = man.section
    def tagline = man.tagline
    def html = man.html
  }

  class Writer(man: Manual) {
    def template = "template.mustache"

    private lazy val mf = new DefaultMustacheFactory {
      override def getObjectHandler() =
        new com.twitter.mustache.ScalaObjectHandler
    }
    private lazy val must = mf.compile(template)

    def write() {
      must.execute(
        new FileWriter(man.basename("html")),
        Page(man)).flush()
    }
 }
}
