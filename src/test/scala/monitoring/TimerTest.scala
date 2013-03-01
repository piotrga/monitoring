package monitoring

import org.scalatest.FreeSpec
import org.scalatest.matchers.MustMatchers
import akka.actor.ActorSystem
import concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


class TimerTest extends FreeSpec with MustMatchers{
  implicit val actorSystem = ActorSystem("test")
  trait X{
    def sleep50()
    def sleep(ms: Long)
    def print(x:String) : Future[Unit]
  }
  class X1 extends X{
    def sleep50() { Thread.sleep(50) }
    def sleep(ms: Long) { Thread.sleep(ms) }
    def print(x:String) = Future{ sleep(20); print(x)}
  }

  "Timer wraps a class" in {
    var timings = List[(String,Long)]()
    val x = Timer.timeClass[X](new X1)((clazz, method, duration) => timings :+= (clazz + "." + method -> duration))
    x.sleep(10)
    x.sleep50()
    timings(0)._1 must be("X1.sleep")
    timings(0)._2 must be >=(10L)
    timings(0)._2 must be <(15L)
    timings(1)._1 must be("X1.sleep50")
  }

  "Timer wraps a class with Future result" in {
    var timings = List[(String,Long)]()
    val x = Timer.timeClass[X](new X1)((clazz, method, duration) => timings :+= (clazz + "." + method -> duration))
    x.print("lalal")
    Thread.sleep(30)
    timings(0)._1 must be("X1.print")
    timings(0)._2 must be >=(20L)
  }
}
