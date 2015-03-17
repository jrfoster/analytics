package com.orionhealth.datamining

import scala.annotation.migration
import scala.collection.mutable.ArraySeq
import scala.collection.JavaConversions.mapAsScalaMap
import scala.math.Ordering
import scala.collection.mutable.ArrayBuffer

object Exercise6b {
  def main(args: Array[String]) = {
    println("Output for part (a)")

    // Generate the data for each basket.  Each basket contains a set of all
    // integers that can be evenly divided by the index of the basket.  As an
    // example, Basket 12 would consist of {12,24,36,48,60,72,84,96} because
    // index 12 evenly divides those integers in the set {1 <= n <= 100}
    val baskets: Array[Seq[Int]] = new Array[Seq[Int]](100)
    for (i <- 1 to 100) {
      baskets(i - 1) = (1 to 100).filter(dividend => dividend % i == 0)
    }

    // This generates the candidate C1 set
    val C1 = new java.util.HashMap[Int, Int]().withDefaultValue(0)
    baskets.foreach(basket => {
      basket.foreach(item => C1.update(item, C1(item) + 1))
    })
    // Pruning C1 to produce the L1 set
    val L1 = C1.filter(kvp => kvp._2 >= 5).keys.toSeq.sorted
    println("Found " + L1.size + " frequent singles")

    // This generates the candidates for k=2 (C2)
    val C2 = new java.util.HashMap[(Seq[Int]), Int]().withDefaultValue(0)
    baskets.foreach(basket => {
      L1.combinations(2).foreach(cmb => {
        if (cmb.toSet.subsetOf(basket.toSet)) {
          C2.update(cmb, C2(cmb) + 1)
        }
      })
    })
    // Pruning C2 to produce the L2 set
    val L2 = C2.filter(kvp => kvp._2 >= 5).keys.toSeq.sortBy(Seq[Int])(seqSorter)
    println("Found " + L2.size + " frequent doubles")

    // This generates the candidates for k=3 (C3)
    val C3 = new java.util.HashMap[(Seq[Int]), Int]().withDefaultValue(0)
    baskets.foreach(basket => {
      selfJoin(L2, 3).foreach(cmb => {
        if (cmb.toSet.subsetOf(basket.toSet)) {
          C3.update(cmb, C3(cmb) + 1)
        }
      })
    })
    // Pruning C3 to produce the L3 set
    val L3 = C3.filter(kvp => kvp._2 >= 5).keys.toSeq.sortBy(Seq[Int])(seqSorter)
    println("Found " + L3.size + " frequent triples")


    // This generates the candidates for k=4 (C4)
    val C4 = new java.util.HashMap[(Seq[Int]), Int]().withDefaultValue(0)
    baskets.foreach(basket => {
      selfJoin(L3, 4).foreach(cmb => {
        if (cmb.toSet.subsetOf(basket.toSet)) {
          C4.update(cmb, C4(cmb) + 1)
        }
      })
    })
    // Pruning C4 to produce the L4 set
    val L4 = C4.filter(kvp => kvp._2 >= 5).keys.toSeq.sortBy(Seq[Int])(seqSorter)
    println("Found " + L4.size + " frequent quadruples")
    
    // This generates the candidates for k=5 (C5)
    val C5 = new java.util.HashMap[(Seq[Int]), Int]().withDefaultValue(0)
    baskets.foreach(basket => {
      selfJoin(L4, 5).foreach(cmb => {
        if (cmb.toSet.subsetOf(basket.toSet)) {
          C5.update(cmb, C5(cmb) + 1)
        }
      })
    })
    // Pruning C5 to produce the L5 set
    val L5 = C5.filter(kvp => kvp._2 >= 5).keys.toSeq.sortBy(Seq[Int])(seqSorter)
    println("Found " + L5.size + " frequent quintuples")
    
    // This generates the candidates for k=6 (C6)
    val C6 = new java.util.HashMap[(Seq[Int]), Int]().withDefaultValue(0)
    baskets.foreach(basket => {
      selfJoin(L5, 6).foreach(cmb => {
        if (cmb.toSet.subsetOf(basket.toSet)) {
          C6.update(cmb, C6(cmb) + 1)
        }
      })
    })
    // Pruning C6 to produce the L6 set
    val L6 = C6.filter(kvp => kvp._2 >= 5).keys.toSeq.sortBy(Seq[Int])(seqSorter)
    println("Found " + L6.size + " frequent sextuples")
    
     // This generates the candidates for k=7 (C7)
    val C7 = new java.util.HashMap[(Seq[Int]), Int]().withDefaultValue(0)
    baskets.foreach(basket => {
      selfJoin(L6, 7).foreach(cmb => {
        if (cmb.toSet.subsetOf(basket.toSet)) {
          C7.update(cmb, C7(cmb) + 1)
        }
      })
    })
    // Pruning C7 to produce the L7 set
    val L7 = C7.filter(kvp => kvp._2 >= 5).keys.toSeq.sortBy(Seq[Int])(seqSorter)
    println("Found " + L7.size + " frequent heptuples")
    
     // This generates the candidates for k=8 (C8)
    val C8 = new java.util.HashMap[(Seq[Int]), Int]().withDefaultValue(0)
    baskets.foreach(basket => {
      selfJoin(L7, 8).foreach(cmb => {
        if (cmb.toSet.subsetOf(basket.toSet)) {
          C8.update(cmb, C8(cmb) + 1)
        }
      })
    })
    // Pruning C8 to produce the L8 set
    val L8 = C8.filter(kvp => kvp._2 >= 5).keys.toSeq.sortBy(Seq[Int])(seqSorter)
    println("Found " + L8.size + " frequent octuples")   
    
     // This generates the candidates for k=9 (C9)
    val C9 = new java.util.HashMap[(Seq[Int]), Int]().withDefaultValue(0)
    baskets.foreach(basket => {
      selfJoin(L8, 9).foreach(cmb => {
        if (cmb.toSet.subsetOf(basket.toSet)) {
          C9.update(cmb, C9(cmb) + 1)
        }
      })
    })
    // Pruning C9 to produce the L9 set
    val L9 = C9.filter(kvp => kvp._2 >= 5).keys.toSeq.sortBy(Seq[Int])(seqSorter)
    println("Found " + L9.size + " frequent nonuples")      

  }  
  
  
  def seqSorter[A <% Ordering[A]]: Ordering[Seq[Int]] = {
    new Ordering[Seq[Int]] {
      override def compare(x: Seq[Int], y: Seq[Int]) = {
        if (x.size != y.size)
          x.size - y.size
        else if (x.size == 1 || x.head != y.head)
          x.head - y.head
        else
          compare(x.drop(1), y.drop(1))
      }
    }
  }

  def selfJoin(left: Seq[Seq[Int]], k: Int): Seq[Seq[Int]] = {
    val rv = ArrayBuffer[Seq[Int]]()
    val right = left
    left.foreach(lTup => {
      right.foreach(rTup => {
        if (lTup != rTup && lTup.take(k - 2) == rTup.take(k - 2))
          rv += lTup.union(rTup).distinct.sorted
      })
    })
    rv.distinct
  }
}