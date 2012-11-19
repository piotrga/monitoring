package monitoring

private[monitoring] case class Stats( epoch:Long, total:Double, sampleCount: Long, max: Double, min: Double ){
  def average = if(sampleCount>0) total/sampleCount else 0d
}

private[monitoring] class Cell{
  // These fields are mutable to avoid garbage collection due to duration monitoring
  private var epoch:Long = 0
  private var totalValue:Double = 0
  private var sampleCount: Long = 0
  private var max: Double = 0
  private var min: Double = 0

  def update(now : Long, sample : Double){
    synchronized{
      if (epoch == now){
        sampleCount += 1
        totalValue += sample
        if (max < sample) max = sample
        if (min > sample) min = sample
      }else{
        epoch = now
        sampleCount = 1
        totalValue = sample
        max = sample
        min = sample
      }
    }
  }

  def snapshot : Stats = synchronized{ Stats(epoch, totalValue, sampleCount, max, min) }
}

class MovingValue(clock: () => Long = System.currentTimeMillis, keepSeconds: Int = 60){

  val N : Int = keepSeconds
  val buff = Array.fill(N)(new Cell)
  val started = NOW()

  private def NOW() = clock() / 1000

  def record(sample: Double){
    val now = NOW()
    val i = (now % N).toInt
    buff(i).update(now, sample)
  }

  private def snapshot = {
    val cutoff = NOW() - N
    buff.map(_.snapshot).filter(_.epoch > cutoff)
  }

  def max = snapshot.map(_.max).max
  def maxPerSecond = snapshot.map(_.total).max

  def min = snapshot.map(_.min).min
  def minPerSecond = snapshot.map(_.total).min

  def sampleCount = snapshot.map(_.sampleCount).sum
  def total = snapshot.map(_.total).sum

  def average : Double = {
    val (duration, count) = snapshot.foldLeft((0d,0d)){case((totalDuration, totalCount),c) => (totalDuration+c.total, totalCount+c.sampleCount)}
    if (count == 0) 0L
    else duration/count
  }

  def averagePerSecond: Double = {
    val secondsMeasured = math.min (started - NOW(), keepSeconds)
    if (secondsMeasured > 0 )
      total / secondsMeasured
    else
      0
  }

}
