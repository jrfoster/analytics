package org.jrfoster.datamining

object Exercise1 {
  
  def main(args: Array[String]) = {
    val n = 20 // This is the number of items
    for (i <- 1 to n) {
      for (j <- i + 1 to n) {
        var k:Int = (((i - 1) * (n - (i / 2f))) + j - i).toInt
        if (k == 100)
          println("when k is 100, i=" + i + ",j=" + j)
      }
    }
  }
}
