package manuel

import scala.util.parsing.input.{ NoPosition, Position }
import com.tristanhunt.knockoff.{ Block, Span }

case class RichHeader(level: Int,
                      attrs: Map[String, String],
                      spans: Seq[Span],
                      position: Position) extends Block

case class RichParagraph(attrs: Map[String, String],
                         spans: Seq[Span],
                         position: Position) extends Block
