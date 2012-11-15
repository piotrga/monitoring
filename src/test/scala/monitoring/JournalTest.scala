package monitoring

import org.scalatest.FreeSpec
import org.scalatest.matchers.MustMatchers
import akka.dispatch.Future
import akka.actor.ActorSystem
import java.net.URL
import io.Source

class JournalTest extends FreeSpec with MustMatchers{
  implicit val sys = ActorSystem("test")
  "Visual ok test" in {
    val journal = Journal()
    journal("Sleep 2000", Future{ Thread.sleep(2000) })
    journal("5+5", Future{ 5+ 5 })

    journal("Fetching gazeta.pl", Source.fromInputStream(new URL("http://www.gazeta.pl").openStream()))
    journal("5/0", Future{ 5/0 })
    journal("5+5", Future{ 5+ 5 })
    journal("Sleep 100", Future{ Thread.sleep(100) })
    Thread.sleep(10)
    println(journal mkString "\n")

  }


}