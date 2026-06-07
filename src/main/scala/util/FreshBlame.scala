package util

import lang.{BlameId, BlameLabel}

import java.util.concurrent.atomic.AtomicInteger

object FreshBlame:
  private val counter = AtomicInteger(0)
  def apply(): BlameLabel = List(BlameId(s"alloc_${counter.getAndIncrement()}"))