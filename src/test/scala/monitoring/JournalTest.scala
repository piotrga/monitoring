/*
 * Copyright 2012 2ndlanguage Limited.
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package monitoring

import org.scalatest.FreeSpec
import org.scalatest.matchers.MustMatchers
import akka.dispatch.{Await, Future}
import akka.actor.ActorSystem
import java.net.URL
import io.Source
import akka.util.duration._

class JournalTest extends FreeSpec with MustMatchers{

  trait X {
    def callA(a: Int, b : String)(implicit journal : Journal) : Unit
    def callB(a: Seq[Int]) : Unit
  }

  class XClass extends X{
    def callA(a: Int, b : String)(implicit journal : Journal) = {
      journal.info("some inside A")
      Thread.sleep(5)
    }
    def callB(a: Seq[Int]) = Thread.sleep(10)
  }

  implicit val system = ActorSystem("test")

  "Visual ok test" in {
    implicit val journal = Journal()
    val x = journal.traceClass[X](new XClass)

    journal("Sleep 2000", Future{ Thread.sleep(2000) })
    journal("5+5", Future{ 5+ 5 })
    x.callA(1, "abc")
    journal("Fetching gazeta.pl")(Source.fromInputStream(new URL("http://www.gazeta.pl").openStream()))
    journal("5/0", Future{ 5/0 }).recover{ case _ => 0}
    journal("5+5", Future{ 5+ 5 })
    x.callB(Seq(1,2,3))
    journal("Sleep 100", Future{ Thread.sleep(100) })

    try
      journal.reportIfFails(System.err, "Buggy stuff"){
        sys.error("some excepion")
      }
    catch {case _ => ()}

    Thread.sleep(10)

    println(journal mkString )

  }


  "Demo" ignore {
    implicit val journal = Journal()
    val allTogetger = for {
      guardian <- Future{Source.fromInputStream(new URL("http://www.guardian.com").openStream())} as "Fetching guardian.com"
      bbc <- Future{Source.fromInputStream(new URL("http://www.bbc.co.uk").openStream())} as "Fetching bbc.co.uk"
      nytimes <- Future{Source.fromInputStream(new URL("http://www.nytimes.com").openStream())} as "Fetching nytimes.com"
    } yield(bbc.getLines().toList ++ guardian.getLines().toList ++ nytimes.getLines().toList)

    {
      val lines = Await.result(allTogetger, 1100 millis)
      val linesWithObama = lines filter (_.contains("Obama")) mkString ("\n") as "Grepping lines"
      println(linesWithObama)
    } onErrorLog (System.err, "Grepping for 'Obama'")

    println(journal.mkString)
  }
  //    Error [Futures timed out after [1100] milliseconds] while executing [Grepping for 'Obama']
  //    Log:
  //      [     5 ms]  Fetching guardian.com - future START
  //      [  1004 ms]  Fetching guardian.com - future DONE in [1000 ms]
  //      [  1005 ms]  Fetching bbc.co.uk - future START
  //    Unfinished futures:
  //      Fetching bbc.co.uk - started on [1005 ms] [114 ms] ago  }
}
