package monitoring

import org.scalatest.FreeSpec
import org.scalatest.matchers.MustMatchers

class MovingValueTest extends  FreeSpec with MustMatchers{
 
  "Empty stats calculate " - {
    val empty = new MovingValue()

    "min" in { empty.min must be(0) }
    "max" in { empty.max must be(0) }
    "average" in { empty.average must be(0) }
    "total" in { empty.total must be(0) }
    "sampleCount" in { empty.sampleCount must be(0) }

    "maxPerSecond" in { empty.maxPerSecond must be(0) }
    "minPerSecond" in { empty.minPerSecond must be(0) }
    "averagePerSecond" in { empty.averagePerSecond must be(0) }
  }

  "The same second stats" - {
    val stats = new MovingValue(() => 12000345)

    stats.record(2)
    stats.record(3)

    "min" in { stats.min must be(2) }
    "max" in { stats.max must be(3) }
    "average" in { stats.average must be(2.5) }
    "total" in { stats.total must be(5) }
    "sampleCount" in { stats.sampleCount must be(2) }

    "maxPerSecond" in { stats.maxPerSecond must be(5) }
    "minPerSecond" in { stats.minPerSecond must be(5) }
    "averagePerSecond" in { stats.averagePerSecond must be(0) }
  }

  "As the time passes keeps data per second" - {
    var time = 12000345
    val stats = new MovingValue(() => time)

    stats.record(1)
    stats.record(1)
    stats.record(3)
    time +=1000
    stats.record(3)
    time +=1000


    "min" in { stats.min must be(2) }
    "max" in { stats.max must be(3) }
    "average" in { stats.average must be(2.5) }
    "total" in { stats.total must be(5) }
    "sampleCount" in { stats.sampleCount must be(2) }

    "maxPerSecond" in { stats.maxPerSecond must be(3) }
    "minPerSecond" in { stats.minPerSecond must be(2) }
    "averagePerSecond" in { stats.averagePerSecond must be(2.5) }
  }

  "Can record events in the past" - {
    var time = 100000
    val stats = new MovingValue(() => time)

    stats.record(2, 100000)
    time += 2000
    stats.record(3, 101000)

    "min" in { stats.min must be(2) }
    "max" in { stats.max must be(3) }
    "average" in { stats.average must be(2.5) }
    "total" in { stats.total must be(5) }
    "sampleCount" in { stats.sampleCount must be(2) }

    "maxPerSecond" in { stats.maxPerSecond must be(3) }
    "minPerSecond" in { stats.minPerSecond must be(2) }
    "averagePerSecond" in { stats.averagePerSecond must be(2.5) }
  }

  "Drops values beyond the window" - {
    var time = 100000
    val stats = new MovingValue(() => time, keepSeconds = 5)

    stats.record(2)
    time += 60000
    stats.record(1, time - 400)
    stats.record(1, time - 300)
    stats.record(1, time - 150)

    "min" in { stats.min must be(1) }
    "max" in { stats.max must be(1) }
    "average" in { stats.average must be(1) }
    "total" in { stats.total must be(3) }
    "sampleCount" in { stats.sampleCount must be(3) }

    "maxPerSecond" in { stats.maxPerSecond must be(3) }
    "minPerSecond" in { stats.minPerSecond must be(3) }
    "averagePerSecond" in { stats.averagePerSecond must be(0.6) }
  }

  
}
