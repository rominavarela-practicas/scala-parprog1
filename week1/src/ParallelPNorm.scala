object ParallelPNorm {
  def apply(vector:List[Int], p:Double) = new ParallelPNorm().pNorm(vector.toArray, p)
  def apply(vector:Array[Int], p:Double) = new ParallelPNorm().pNorm(vector, p)
}

class ParallelPNorm {
  def sumSegment(vector:Array[Int], p:Double, fromIndex:Int, toIndex:Int): Double = {
    assert(fromIndex >= 0, "Math Processing Error")
    assert(toIndex <= vector.length, "Math Processing Error")
    assert(fromIndex <= toIndex, "Math Processing Error")
    
    vector.slice(fromIndex, toIndex)
      .foldLeft(0D) { (accum, vi) =>
        accum + Math.pow(vi, p)
      }
  }
  
  def parallel[T](f1: => T, f2: => T): (T, T) = {
    var result1: Option[T] = None
    var result2: Option[T] = None
    
    val t1 = new Thread {
      override def run() {
        result1 = Some(f1)
      }
    }
    val t2 = new Thread {
      override def run() {
        result2 = Some(f2)
      }
    }
    
    t1.start()
    t2.start()
    t1.join()
    t2.join()
    
    (result1.get, result2.get)
  }
  
  private def _pNorm(vector:Array[Int], p:Double): Double = {
    if(vector.length > 1) {
      val (v1, v2) = vector.splitAt(vector.length / 2)
      val (seg1, seg2) = parallel (
          _pNorm(v1, p),
          _pNorm(v2, p)
      )
      seg1 + seg2
    } else {
      sumSegment(vector, p, 0, vector.length)
    }
  }
  
  def pNorm(vector:Array[Int], p:Double): Double = {
    val segmentSum = _pNorm(vector, p)
    Math.pow(segmentSum, 1/p)
  }
}