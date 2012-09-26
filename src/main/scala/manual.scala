package manuel

import com.tristanhunt.knockoff.DefaultDiscounter.knockoff
import com.tristanhunt.knockoff._
import io.Source

import java.io.File

case class Manual(base: File) {

  private lazy val data = Source.fromFile(base).getLines.mkString("\n")

  private lazy val ast: Seq[Block] = knockoff(data)

  val (sname, ssection, tagline) = extractMeta

  val version = "0.1.0"

  val name = sname.orElse(pathName)

  val section = ssection.orElse(pathSection)

  val basename = base.getPath.split(File.separator).last

  val index = Index(base).include(this)

  val title = name.map(_ + section.map("(" + _ + ")")
                  .getOrElse(""))
                  .map(_ + " - ")
                  .getOrElse("") + tagline

  def date = base.lastModified()

  def manual = Some(title)

  /** name(1).kind */
  def basename(kind: String) = Seq(
    pathName.orElse(sname),
    pathSection.orElse(ssection),
    kind match {
      case "roff" | "" => None
      case k => Some(k)
    }).flatten.mkString(".")

  /** name.(_)._ */
  def pathName = {
    val Name = """\.(\d\w*)\.""".r
    base.getName() match {
      case Name(n) => Some(n)
      case _ => None
    }
  }

  /** _.(1)._*/
  def pathSection = {
    val Section = """\.(\d\w*)\.""".r
    base.getName() match {
      case Section(s) => Some(s)
      case _ => None
    }
  }

  /** name(1) */
  def referenceName = section match {
    case Some(s) => "%s(%s)" format(name.get, s)
    case _ => name.get
  }

  /** list of (section-id, section name)*/
  def toc = withName(ast).collect {
    case Header(2, Text(sect) :: Nil, _) =>
      (sect.replaceAll("""\W+""", "-"), sect)
  }

  def filteredMarkdown =
    anchoredHeaders(withName(withoutHeader(ast)))

  def withName(ast: Seq[Block]): Seq[Block] =
    Seq(Header(2, Seq(Text("NAME")), null),
        Paragraph(Seq(CodeSpan(name.get),
                      Text(" - %s" format tagline)), null)) ++ ast

  def withoutHeader(ast: Seq[Block]) =
    ast.filterNot {
      case Header(1, _, _) => true
      case _ => false
    }

  def anchoredHeaders(ast: Seq[Block]) =
    ast map {
      case Header(num, spans @ Text(sect) :: Nil, pos) =>
        RichHeader(num, Map("id" -> sect.replaceAll("""\W+""", "-")),
                   spans, pos)
      case blk => blk
    }

  def markdown = filteredMarkdown

  def html = ManuelDiscounter.toXHTML(filteredMarkdown)

  private def extractMeta = {
    // name(section) -- description
    val NameSecTagline = """([\w_.\[\]~+=@:-]+)\s*\((\d\w*)\)\s*-+\s*(.*)""".r
    // name -- description
    val NameTagline = """([\w_.\[\]~+=@:-]+)\s+-+\s+(.*)""".r
    ast.collectFirst({
      case Header(1, Text(txt) :: Nil, _) => txt
    }) match {
      case Some(txt) =>
        txt match {
          case NameSecTagline(name, sect, tagl) => (Some(name), Some(sect), tagl)
          case NameTagline(name, tagl) =>  (Some(name), None, tagl)
          case tagl => (None, None, tagl)
        }
      case _ => (None, None, "")
    }
  }
}
