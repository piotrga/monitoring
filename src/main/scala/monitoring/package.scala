import akka.dispatch.Future
import java.io.{PrintStream, PrintWriter}
import java.util.logging.{Level, Logger => JavaLogger}

package object monitoring {

  implicit def PrintStream2Log(out : PrintStream) = new {
    def error(msg: String, ex: Throwable) {
      out.println(msg)
      ex.printStackTrace(out)
    }
  }

  implicit def PrintWriter2Log(out : PrintWriter) = new {
    def error(msg: String, ex: Throwable) {
      out.println(msg)
      ex.printStackTrace(out)
    }
  }

  implicit def java_util_logging_Logger2Log(out : JavaLogger) = new {
    def error(msg: String, ex: Throwable) {
      out.log(Level.SEVERE, msg, ex)
    }
  }

  implicit def JournaledFuture[T](f: Future[T])(implicit journal : Journal) = new {
    def as(msg : String) = journal(msg, f)
  }

  implicit def JournaledBlock[T](f: => T)(implicit journal : Journal) = new {
    def as(msg : String) = journal(msg)(f)
    def reportIfFails[T, LOG <% { def error(msg: String, ex: Throwable) : Unit}](log : LOG, description:String = "operation") = journal.reportIfFails(log, description)(f)
    def reportIfFails[T](description:String = "operation")(implicit log : { def error(msg: String, ex: Throwable) : Unit}) = journal.reportIfFails(log, description)(f)
  }

}
