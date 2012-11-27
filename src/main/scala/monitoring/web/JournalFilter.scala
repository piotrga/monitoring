package monitoring.web

import monitoring.Journal

import javax.servlet.Filter
import javax.servlet.ServletRequest
import javax.servlet.ServletResponse
import javax.servlet.FilterChain
import javax.servlet.FilterConfig

object JournalPerThread{
  private val tl = new ThreadLocal[Journal]()

  def get : Option[Journal] = Option(tl.get)

  def getOrCreate() = {
    if (tl.get() == null) tl.set(new Journal)
    tl.get()
  }

  def reset(){ tl.set(null) }
}

class JournalFilter extends Filter{

  def doFilter(request: ServletRequest, response: ServletResponse, chain: FilterChain) {
    try chain.doFilter(request, response) finally JournalPerThread.reset()
  }

  def init(filterConfig: FilterConfig) {}
  def destroy() {}
}