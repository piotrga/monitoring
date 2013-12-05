## Example usage:
```scala
    implicit val journal = Journal()
    val allTogether = for {
      guardian <- Future{Source.fromInputStream(new URL("http://www.guardian.com").openStream())} as "Fetching guardian.com"
      bbc <- Future{Source.fromInputStream(new URL("http://www.bbc.co.uk").openStream())} as "Fetching bbc.co.uk"
      nytimes <- Future{Source.fromInputStream(new URL("http://www.nytimes.com").openStream())} as "Fetching nytimes.com"
    } yield(bbc.getLines().toList ++ guardian.getLines().toList ++ nytimes.getLines().toList)

    {
      val lines = Await.result(allTogether, 1100 millis)
      val linesWithObama = lines filter (_.contains("Obama")) mkString ("\n") as "Grepping lines"
      println(linesWithObama)
    } onErrorLog (System.err, "Grepping for 'Obama'")
```

## Example output:
    Error [Futures timed out after [1100] milliseconds] while executing [Grepping for 'Obama']
    Log:
      [     5 ms]  Fetching guardian.com - future START
      [  1004 ms]  Fetching guardian.com - future DONE in [1000 ms]
      [  1005 ms]  Fetching bbc.co.uk - future START
    Unfinished futures:
      Fetching bbc.co.uk - started on [1005 ms] [114 ms] ago
