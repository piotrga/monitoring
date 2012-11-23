package monitoring

import atomic.Atomic
import akka.dispatch.Future
import java.lang.reflect.{InvocationTargetException, Method, InvocationHandler}

case class Timer(time : () => Long = () =>  System.currentTimeMillis()) {
  val start = time()
  def duration: Long = time() - start
}

object Timer{
  def apply(relativeTo : Timer) : Timer = Timer(()=> System.currentTimeMillis() - relativeTo.start)

  def time[T]( f: => T) : (T, Long)  ={
    val t = Timer()
    val res = f
    (res, t.duration)
  }
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

  def info(message: => String) {
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
    info("%s - future START" format description)
    val f = future
    unfinishedFutures.update(m => m + (f -> (description, futureTimer)))
    f andThen {
      case Left(ex) => error("%s - future FAILED after [%d ms]" format (description, futureTimer.duration), ex)
      case Right(_) => info("%s - future DONE in [%d ms]" format(description, futureTimer.duration))
    } andThen {
      case _ => unfinishedFutures.update( m => m - f )
    }
  }

  def apply[T](description: String)(f: => T): T ={
    val timer = Timer()
    info("%s - START" format description)
    try{
      val res = f
      info("%s - DONE in [%d ms]" format (description, timer.duration))
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

  def mkString = {
    val msgs = log.get()
    val unfinished = unfinishedFutures.get()

    val messages = msgs.reverse map (_.mkString) mkString ("\n\t")
    "Log:\n\t%s\nUnfinished futures:\n\t%s\n" format (messages, unfinished.values.map{case (desc, timer) => "%s - started on [%d ms] [%d ms] ago" format (desc, timer.start, timer.duration)}.mkString("\n\t"))
  }


  def traceClass[T](target: T)(implicit mf: ClassManifest[T]): T = {
    java.lang.reflect.Proxy.newProxyInstance(mf.erasure.getClassLoader, Array(mf.erasure.asInstanceOf[Class[T]]), new InvocationHandler {
      val targetName = target.getClass.getSimpleName.replaceAll("[^a-zA-Z]", "_")
      def invoke(p1: AnyRef, method: Method, args: Array[AnyRef]) = {
        apply(targetName + "." + method.getName + "(...)") {
          try {
            method.invoke(target, args: _*)
          }
          catch {
            case ex: InvocationTargetException => {
              ex.getTargetException match {
                case e: RuntimeException => throw e
                case e: Throwable if method.getExceptionTypes.contains(e.getClass) => throw e
                case e: Throwable => throw new CheckedExceptionWrapper(e.getMessage, e)
              }
            }
          }
        }
      }
    }).asInstanceOf[T]
  }

  def reportIfFails[T, LOG <% { def error(msg: String, ex: Throwable) : Unit}](log : LOG, description:String = "operation")(f: => T)  : T = {
    implicit val journal : Journal = this
    try
      f
    catch{
      case ex: Throwable =>
        log.error("Error [%s] while executing [%s]\n%s" format (ex.getMessage, description, this.mkString), ex )
        throw ex
    }
  }

}

class CheckedExceptionWrapper(msg : String, cause : Throwable) extends RuntimeException(msg, cause)