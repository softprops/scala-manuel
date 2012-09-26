package manuel

import com.tristanhunt.knockoff._

object ManuelDiscounter extends Discounter {
  import scala.xml.{ UnprefixedAttribute => Attr,
                    Elem, Node, Text => XmlText,
                    Null => Empty }

  override def blockToXHTML: Block => Node =
    block: Block => block match {
      case RichHeader(level, attrs, spans, _) =>
        richHeaderToXHTML(level, attrs, spans)
      case default => super.blockToXHTML(default)
    }

  def richHeaderToXHTML: (Int, Map[String, String], Seq[Span]) => Node =
    (level, attrs, spans) => {
      (super.headerToXHTML(level, spans) /: attrs) {
        case (el : Elem, (k, v)) =>
          el % new Attr(k, v, Empty)
      }
    }
}
