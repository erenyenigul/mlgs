package lang
import scala.collection.immutable.Set

case class BlameId(id: String, line: Int = 0, col: Int = 0, sourceLine: String = "")
type BlameLabel = Set[BlameId]
