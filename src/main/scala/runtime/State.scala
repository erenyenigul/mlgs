package runtime

import lang.*
import lang.SecurityLevel.L

import scala.collection.mutable

case class HeapCell(t: Type, p: BlameLabel, v: Value)
type Heap = mutable.Map[String, HeapCell]

case class State (pc: SecurityLevel = L, heap: Heap = mutable.Map()):
  def withAllocation(loc: String, cell: HeapCell): State =
    this.copy(heap = this.heap + (loc -> cell))

  def withPC(newPC: SecurityLevel): State =
    this.copy(pc = newPC)