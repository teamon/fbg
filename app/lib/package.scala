package object lib {
  def ensureWidth(str: String, width: Int, span: String) = {
    def aux(as: List[String], bs: List[String]): List[String] = (as, bs) match {
      case (a :: as,  b :: bs) if (a + b).length >= width => aux(as, a :: b :: bs)
      case (a :: as,  b :: bs) => aux(as, (b + " " + a) :: bs)
      case (a :: as,  bs)      => aux(as, a :: bs)
      case (Nil,      bs)      => bs.reverse
    }

    str.split("\n").flatMap { line =>
      aux(line.split("\\s").toList, Nil)
    }.mkString("\n" + span)
  }

  val autoAnchorRe = "(?:https?|ftps?)://[\\w/%.-][/\\??\\w=?\\w?/%.-]?[/\\?&\\w=?\\w?/%.-]*#?[\\w/%.-:!]*"

  def autoAnchor(str: String) = scala.xml.Utility.escape(str).replaceAll(autoAnchorRe, "<a href=\"$0\">$0</a>")
}
