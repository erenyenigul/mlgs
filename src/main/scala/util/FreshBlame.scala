package util

import lang.{BlameId, BlameLabel}

import java.util.concurrent.atomic.AtomicInteger

object FreshBlame:
  private val counter = AtomicInteger(0)
  def apply(prefix: String = "blame", line: Int = 0, col: Int = 0, sourceLine: String = ""): BlameLabel =
    List(BlameId(s"${prefix}_${counter.getAndIncrement()}", line, col, sourceLine))
