package util

import java.util.concurrent.atomic.AtomicInteger

object FreshLoc:
  private val counter = AtomicInteger(0)
  def apply(): String = s"loc_${counter.getAndIncrement()}"