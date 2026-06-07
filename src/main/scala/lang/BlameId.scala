package lang

case class BlameId(id: String, line: Int = 0, col: Int = 0, sourceLine: String = "")
type BlameLabel = List[BlameId]
