import java.util.Date

object Main extends App {
  def hr = println("\n----------\n")
  
  val vector = 3/2 :: 0 :: 1/2 :: 
                0  :: 3 ::  0  :: 
               1/2 :: 0 :: 3/2 ::
                Nil
  
  var time1:Date = _
  var time2:Date = _
                
  time1 = new Date()
  println(PNorm(vector, 3))
  time2 = new Date()
  println(s"sequential, time = ${time2.getTime - time1.getTime} ms")
  
  hr
  
  time1 = new Date()
  println(TwoThreadsPNorm(vector, 3))
  time2 = new Date()
  println(s"two threads, time = ${time2.getTime - time1.getTime} ms")
  
  hr
  
  time1 = new Date()
  println(ParallelPNorm(vector, 3))
  time2 = new Date()
  println(s"${vector.length} threads, time = ${time2.getTime - time1.getTime} ms")
}