package monitoring

import atomic.Atomic
import java.lang.reflect.{InvocationTargetException, Method, InvocationHandler}
import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.JavaConversions._
import concurrent.{ExecutionContext, Future}
import util.{Success, Failure}
import ExecutionContext.Implicits.global

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

  def time[T]( f: => Future[T], record : Long => Unit)(implicit exec : ExecutionContext) : Future[T]  ={
    val t = Timer()
    f andThen { case _ => try record(t.duration) catch {case ignore => ()}}
  }


  def time[T]( f: => T, record : Long => Unit) : T  ={
    val (res, duration) = time(f)
    try record(duration) catch {case ignore => ()}
    res
  }

  def timeClass[T](target: T)(record : (String, String, Long) => Unit)(implicit mf: ClassManifest[T]): T = {
    java.lang.reflect.Proxy.newProxyInstance(mf.erasure.getClassLoader, Array(mf.erasure.asInstanceOf[Class[T]]), new InvocationHandler {
      val targetName = target.getClass.getSimpleName
      def invoke(p1: AnyRef, method: Method, args: Array[AnyRef]) = {
          try {
            if (method.getReturnType.isAssignableFrom(classOf[Future[_]]))
              time(method.invoke(target, args: _*).asInstanceOf[Future[Nothing]], record.curried(targetName)(method.getName))
            else
              time(method.invoke(target, args: _*), record.curried(targetName)(method.getName))
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
    }).asInstanceOf[T]
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

case class Msg(offset:Long, msg: String, typ: MsgType = INFO, stackFrame : Option[StackTraceElement] = None){
  def mkString = {
    val severity = if (typ.mkString.isEmpty) " " else " "+typ.mkString+" in: "
    val res = "[%6d ms]%s%s %s" format(offset, severity, msg, typ.details)
    stackFrame.map(f => res.padTo(100, " ").mkString + "at " + f.toString).getOrElse(res)
  }
}

case class Journal() {
  private val timer = Timer()
  private val log = Atomic(Seq[Msg]())
  private val unfinishedFutures = Atomic(Map[Future[_], (String, Timer, StackTraceElement)]())


  def info(message: => String) { info(message, getStackFrame(2)) }

  private def info(message: => String, stackFrame : Option[StackTraceElement]) {
    val duration = timer.duration
    val msg = Msg(duration, message, stackFrame = stackFrame)
    append(msg)
  }



  def error(message: => String, error : Throwable = null, stackFrame : Option[StackTraceElement] = getStackFrame(2)){
    val msg = Msg(timer.duration, message, ERROR(error), stackFrame)
    append(msg)
  }

  def apply[T](description: String, future: => Future[T]): Future[T] ={
    val futureTimer = Timer(relativeTo = timer)
    val stackFrame = getStackFrame(2)
    info("%s  -  Future START" format description, stackFrame)
    val f = future
    unfinishedFutures.update(m => m + (f -> (description, futureTimer, stackFrame.get)))
    f andThen {
      case Failure(ex) => error("%s  -  Future FAILED after [%d ms]" format (description, futureTimer.duration), ex, stackFrame)
      case Success(_) => info("%s  -  Future DONE in [%d ms]" format(description, futureTimer.duration), stackFrame)
    } andThen {
      case _ => unfinishedFutures.update( m => m - f )
    }
  }

  def apply[T](description: String)(f: => T): T = wrapBlock(description, getStackFrame(2))(f)

  private def wrapBlock[T](description: String, stackFrame : Option[StackTraceElement])(f: => T): T ={
    info("%s - START" format description, stackFrame )
    val timer = Timer()
    try{
      val res = f
      info("%s - DONE in [%d ms]" format (description, timer.duration), stackFrame)
      res
    }catch{
      case ex:Throwable =>
        error("%s failed" format description, ex, stackFrame)
        throw ex
    }
  }

  private def append(msg: Msg) {
    log.update(log => msg +: log)
    //    println(msg)
  }

  val df = new SimpleDateFormat("HH:mm:ss,SSS - dd MMM yyyy")

  def mkString = {
    val msgs = log.get()
    val unfinished = unfinishedFutures.get()

    val messages = msgs.reverse map (_.mkString) mkString ("\n\t")
    val res = "Log(Started at %s):\n\t%s\n" format (df.format(new Date(timer.start)), messages)
    if (unfinished.isEmpty)
      res
    else
      res+"Unfinished futures:\n\t%s\n" format unfinished.values.map{case (desc, timer, frame) => "[%6d ms] %s - started [%d ms] ago at %s" format (timer.start, desc, timer.duration, frame )}.mkString("\n\t")
  }


  def traceClass[T](target: T)(implicit mf: ClassManifest[T]): T = {
    java.lang.reflect.Proxy.newProxyInstance(mf.erasure.getClassLoader, Array(mf.erasure.asInstanceOf[Class[T]]), new InvocationHandler {
      val targetName = target.getClass.getSimpleName.replaceAll("[^a-zA-Z]", "_")
      def invoke(p1: AnyRef, method: Method, args: Array[AnyRef]) = {
        wrapBlock(targetName + "." + method.getName + "(...)", getStackFrame(3)) {
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

  private def getStackFrame(i: Int): Some[StackTraceElement] ={
    val res = Thread.currentThread().getStackTrace.drop(i+1)
    Some(if(res(0).getClassName.startsWith("monitoring.package$")) // ugly hack to remove implicit conversion crap
      res.drop(5)(0)
    else
      res(0)
    )
  }
}

class CheckedExceptionWrapper(msg : String, cause : Throwable) extends RuntimeException(msg, cause)