package manuel

import com.tristanhunt.knockoff.DefaultDiscounter._
import com.tristanhunt.knockoff._
import java.util.Date
import java.text.SimpleDateFormat

import java.io.FileWriter

object Roff {

  val Newline = "\n"

  val Entities = Map(
   "&bull;"  -> """\(bu""",
   "&lt;"    -> "<",
   "&gt;"    -> ">",
   "&nbsp;"  -> """\~""",
   "&copy;"  -> """\(co""",
   "&rdquo;" -> """\(rs""",
   "&mdash;" -> """\(em""",
   "&reg;"   -> """\(rg""",
   "&sec;"   -> """\(sc""",
   "&ge;"    -> """\(>=""",
   "&le;"    -> """\(<=""",
   "&ne;"    -> """\(!=""",
   "&equiv;" -> """\(==""")

  val DateFormat = "MMMM yyyy"

  val ManuelVersion = "0.1.0"

  class Writer(man: Manual) {

    private val sb = new StringBuffer()

    private val html = man.markdown
    
    private val lastModified =
      new SimpleDateFormat(DateFormat).format(new Date(man.date))

    private val topline = Some(""""%s" "%s" "%s" "%s" "%s"""" format(
      escape(man.name.getOrElse("").toUpperCase),
      man.section.getOrElse(""),
      lastModified,
      man.version,
      man.manual.getOrElse("")
    ))

    comment("generated with Manuel/v%s" format ManuelVersion)
    comment("http://github.com/softprops/manuel/tree/%s" format ManuelVersion)
    macro("TH", topline)

    html.zipWithIndex.map(blockFilter)

    override def toString = sb.toString().replaceAll("""[\t]+$""", "")

    private def blockFilter(b: (Block, Int)): Unit = {
      b match {
        case (Header(1, _, _) | RichHeader(1, _, _, _), _) => ()          
        case (Header(2, spans, _), _) =>
          macro("SH", Some(quote(escape(spans.map(unspan).mkString("")))))
        case (RichHeader(2, _, spans, _), _) =>
          macro("SH", Some(quote(escape(spans.map(unspan).mkString("")))))
        case (Header(3, spans, _), _) =>
          macro("SS", Some(quote(escape(spans.map(unspan).mkString("")))))
        case (RichHeader(3, _, spans, _), _) =>
          macro("SS", Some(quote(escape(spans.map(unspan).mkString("")))))
        case (Paragraph(spans, _), at) =>
          html(at - 1) match {
            case Header(1 | 2 | 3, _, _) | RichHeader(1 | 2 | 3, _, _, _) => ()
            case _ => macro("P")
          }
          spans.map(inlineFilter)
        case (Blockquote(children, _), _) =>
          macro("IP")
          children.view.zipWithIndex.map(blockFilter)
        case (CodeBlock(txt, _), _) =>
          inlineFilter(txt)
        case (UnorderedList(items), _) =>
          items.view.zipWithIndex.map(blockFilter)
        case (OrderedList(items), _) =>
          items.view.zipWithIndex.map(blockFilter)
        case (OrderedItem(children, _), _) =>
          children.view.zipWithIndex.map(blockFilter)
        case (UnorderedItem(children, _), _) =>
          children.view.zipWithIndex.map(blockFilter)
        case (l @ LinkDefinition(id, url, title, pos), _) =>
          write(" ")
          writefi {
            write(escape(url))
          }
        case e =>
          throw new Exception("unhandled block element %s" format e)
      }
    }

    private def unspan(s: Span): String = s match {
      case Text(t) => t
      case uh => 
        println("un unspaned %s" format uh)
        uh.toString
    }

    private def writefb(f: => Unit) = {
      write("""\fB""")
      f
      write("""\fR""")
    }

    private def writefi(f: => Unit) = {
      write("""\fI""")
      f
      write("""\fR""")
    }

    private def inlineFilter(s: Span): Unit = s match {
      case Text(t) =>
        if (!t.isEmpty && t != Newline) write(escape(t))
      case HTMLSpan(t) =>
        if (t == "<br>") macro("br")
        else write(escape(t))
      case CodeSpan(txt) =>
        writefb {
          write(escape(txt))
        }
      case Strong(spans) =>
        writefb {
          spans.map(inlineFilter)
        }
      case Emphasis(spans) =>
        writefi {
          spans.map(inlineFilter)
        }
      case ImageLink(spans, url, title) =>
        spans.map(inlineFilter)
        write(" ")
        writefi {
          write(escape(url))
        }
      case IndirectLink(spans, lkdef) =>
        spans.map(inlineFilter)
        write(" ")
        writefi {
          write(escape(lkdef.url))
        }
      case Link(spans, url, title) =>
        spans.map(inlineFilter)
        writefi {
          write(escape(url))
        }
      case e => throw new Exception(
        "unexpected inline element: %s" format e
      )
    }

   private def writeln(text: String) = {
     val res = sb.toString
     if (!res.isEmpty && !res.endsWith(Newline)) write(Newline)
     write(text)
     if (!text.endsWith(Newline)) write(Newline)
   }

   private def isComment(text: String) =
     text.substring(0, 2) == """\."""

   private def write(text: String) =
     if (text.isEmpty) { this }
     else {
       if (text.length > 1 && isComment(text) &&
           sb.toString.endsWith(Newline)) {
             sb.append("""\&""")
           }
       sb.append(text)
       this
     }

   private def quote(text: String) =
     "\"%s\"" format (text.replace("\"", """\""""))

   private def comment(text: String) =
     writeln(""".\" %s""" format text)

   private def macro(name: String,
                     value: Option[String] = None) =
     writeln(".%s.%s" format(
             Newline,
             Seq(Some(name), value).flatten.mkString(" ")))

   private def escape(text: String) =
     if (text.isEmpty) text
     else {
       import scala.util.matching.Regex.Match
       val Special = """(&[A-Za-z]+;)""".r
       val Escape = """(['.-])""".r
       val r = text.replaceAll("""&#x([0-9A-Fa-f]+);""", "$1")
                   .replaceAll("""&#(\d+);""", "$1")
                   .replaceAll("""\\""", """\e""")
                   .replaceAll("&amp;", "&")
                   .replaceAll("""(['.-])""", """\\$1""")
       Escape replaceAllIn(
         Special replaceAllIn(
           r, m => Entities.get(m.group(0)).getOrElse(m.group(0))),
           m => "\\%s" format m)
      }

    def write() {
      val writer = new FileWriter(man.basename(""))
      writer.write(this.toString)
      writer.flush()
      writer.close()
    }
  }
}
