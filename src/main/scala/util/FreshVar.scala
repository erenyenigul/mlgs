package util

import java.util.concurrent.atomic.AtomicInteger

object FreshVar:
  private val counter = AtomicInteger(0)
  def apply(): String = s".var_${counter.getAndIncrement()}"