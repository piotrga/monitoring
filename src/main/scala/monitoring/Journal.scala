package monitoring

import atomic.Atomic
import akka.dispatch.Future

case class Timer(time : () => Long = () =>  System.currentTimeMillis()) {
  val start = time()
  def duration: Long = time() - start
}

object Timer{
  def apply(relativeTo : Timer) : Timer = Timer(()=> System.currentTimeMillis() - relativeTo.start)
}

sealed trait MsgType{
  def mkString : String = ""
  def details : String = ""
}
case object INFO extends MsgType

case class ERROR(cause: Throwable) extends MsgType{
  override val mkString = "ERROR"
  override def details = Option(cause) map(_.toString) getOrElse ""
}

case class Msg(offset:Long, msg: String, typ: MsgType = INFO){
  def mkString = "[%6d ms] %s %s %s" format (offset, typ.mkString, msg, typ.details)
}

case class Journal() {
  private val timer = Timer()
  private val log = Atomic(Seq[Msg]())
  private val unfinishedFutures = Atomic(Map[Future[_], (String, Timer)]())

  def apply(message: => String) {
    val duration = timer.duration
    val msg = Msg(duration, message)
    append(msg)
  }


  def error(message: => String, error : Throwable = null){
    val msg = Msg(timer.duration, message, ERROR(error))
    append(msg)
  }

  def apply[T](description: String, future: => Future[T]): Future[T] ={
    val futureTimer = Timer(relativeTo = timer)
    this("%s - future START" format description)
    val f = future
    unfinishedFutures.update(m => m + (f -> (description, futureTimer)))
    f andThen {
      case Left(ex) => error("%s - future FAILED after [%d ms]" format (description, futureTimer.duration), ex)
      case Right(_) => this("%s - future DONE in [%d ms]" format(description, futureTimer.duration))
    } andThen {
      case _ => unfinishedFutures.update( m => m - f )
    }
  }

  def apply[T](description: String, future: => T): T ={
    val timer = Timer()
    this("%s - START" format description)
    try{
      val res = future
      this("%s - DONE in [%d ms]" format (description, timer.duration))
      res
    }catch{
      case ex:Throwable =>
        error("%s failed" format description, ex)
        throw ex
    }
  }

  private def append(msg: Msg) {
    log.update(log => msg +: log)
//    println(msg)
  }

  def mkString(separator : String) ={
    val msgs = log.get()
    val unfinished = unfinishedFutures.get()

    val messages = msgs.reverse map (_.mkString) mkString ("\n\t")
    "Log:\n\t%s\nUnfinished futures:\n\t%s\n" format (messages, unfinished.values.map{case (desc, timer) => "%s - started on [%d ms] [%d ms] ago" format (desc, timer.start, timer.duration)}.mkString("\n\t"))
  }
}
