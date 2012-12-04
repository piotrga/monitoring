package monitoring

import org.scalatest.FreeSpec
import org.scalatest.matchers.MustMatchers

class TimerTest extends FreeSpec with MustMatchers{

  trait X{
    def sleep50()
    def sleep(ms: Long)
  }
  class X1 extends X{
    def sleep50() { Thread.sleep(50) }
    def sleep(ms: Long) { Thread.sleep(ms) }
  }

  "Timer wraps a class" in {
    var timings = List[(String,Long)]()
    val x = Timer.timeClass[X](new X1)((name, duration) => timings :+= (name -> duration))
    x.sleep(10)
    x.sleep50()
    timings(0)._1 must be("X_.sleep(...)")
    timings(0)._2 must be >=(10L)
    timings(0)._2 must be <(12L)
    timings(1)._1 must be("X_.sleep50(...)")
  }
}
