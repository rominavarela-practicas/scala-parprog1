object PNorm {
  def apply(vector:List[Int], p:Double) = new PNorm().pNorm(vector.toArray, p)
  def apply(vector:Array[Int], p:Double) = new PNorm().pNorm(vector, p)
}

class PNorm {
  def sumSegment(vector:Array[Int], p:Double, fromIndex:Int, toIndex:Int): Double = {
    assert(fromIndex >= 0, "Math Processing Error")
    assert(toIndex <= vector.length, "Math Processing Error")
    assert(fromIndex <= toIndex, "Math Processing Error")
    
    vector.slice(fromIndex, toIndex)
      .foldLeft(0D) { (accum, vi) =>
        accum + Math.pow(vi, p)
      }
  }
  
  def pNorm(vector:Array[Int], p:Double): Double = {
    val sum = sumSegment(vector, p, 0, vector.length)
    Math.pow(sum, 1/p)
  }
}