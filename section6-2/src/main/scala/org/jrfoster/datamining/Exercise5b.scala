package org.jrfoster.datamining

import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.immutable.Seq
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer

object Exercise5b {
  def main(args: Array[String]) = {
    println("Output for part (b)")

    // Generate the data for each basket.  Each basket contains a set of all
    // integers that can be evenly divided by the index of the basket.  As an
    // example, Basket 12 would consist of {12,24,36,48,60,72,84,96} because
    // index 12 evenly divides those integers in the set {1 <= n <= 100}
    val baskets: Array[Seq[Int]] = new Array[Seq[Int]](100)
    for (i <- 1 to 100) {
      baskets(i - 1) = (1 to 100).filter(dividend => dividend % i == 0)
    }
    
    // An itemset is said to be maximal if no superset is frequent, so this
    // implies that we have to generate sets of frequent singles, doubles,
    // triples, quadruples, etc. until we find that there are none, so we know
    // what constitutes an empty superset.  For this exercise, I am using the
    // "triples" methodology for storing counts.  Support for all sets is 5.
    // First, generate frequent singletons
    val count1: java.util.HashMap[Int, Int] = new java.util.HashMap[Int, Int]()
    for (i <- 1 to 100) {
      count1 += i -> baskets.filter(items => Set(i).subsetOf(items.toSet)).size
    }
    // This variable holds the frequent singletons
    val freq1 = count1.filter(kv => kv._2 >= 5).keys.toSeq
    println("Found " + freq1.size + " frequent singletons")

    // Now the doubles. We go through all 100 baskets (because each basket must
    // contain at least the pair {1, n}) and generate the doubles for the items
    // in each basket using only the intersection of the values in the basket
    // and the set of frequent singletons.
    val count2: java.util.HashMap[(Int, Int), Int] = new java.util.HashMap[(Int, Int), Int]()
    baskets.foreach(basket => {
      basket.intersect(freq1).combinations(2).foreach(d => {
        val key: Tuple2[Int,Int] = Tuple2(d(0), d(1))
        if (count2.keySet.contains(key))
          count2(key) += 1
        else
          count2 += key -> 1
      })
    })
    // This variable holds the frequent doubles
    val freq2 = count2.filter(kv => kv._2 >= 5).keySet
    println("Found " + freq2.size + " frequent doubles")

    // Now the triples.  We go through all the baskets, but here we can skip the
    // ones that don't have at least 3 items.  For those we do process, we use
    // the same approach, but this time generating triples. We use the singletons
    // to generate a triple, then generate all doubles for that triple and see if
    // the double is in our collection of frequent doubles.  If it is, we keep
    // it and keep a running count for the triple.
    val count3: java.util.HashMap[(Int, Int, Int), Int] = new java.util.HashMap[(Int, Int, Int), Int]()
    baskets.foreach(basket => {
      val ib = basket.intersect(freq1)
      if (ib.size >= 3) {
        ib.combinations(3).foreach(t => {
          val key: Tuple3[Int,Int,Int] = Tuple3(t(0), t(1), t(2))
          if (t.combinations(2).forall(d => freq2.contains((d(0), d(1))))) {
            if (count3.keySet.contains(key))
              count3(key) += 1
            else
              count3 += key -> 1
          }
        })
      }
    })
    // This variable holds the frequent triples
    val freq3 = count3.filter(kv => kv._2 >= 5).keySet
    println("Found " + freq3.size + " frequent triples")

    // Now the quadruples.  Same basic pattern except we exclude those baskets
    // with fewer than 4 items and we check possible triples against frequent
    // ones.
    val count4: java.util.HashMap[(Int, Int, Int, Int), Int] = new java.util.HashMap[(Int, Int, Int, Int), Int]()
    baskets.foreach(basket => {
      val ib = basket.intersect(freq1)
      if (ib.size >= 4) {
        ib.combinations(4).foreach(q => {
          val key: Tuple4[Int,Int,Int,Int] = Tuple4(q(0), q(1), q(2), q(3))
          if (q.combinations(3).forall(t => freq3.contains((t(0), t(1), t(2))))) {
            if (count4.keySet.contains(key))
              count4(key) += 1
            else
              count4 += key -> 1
          }
        })
      }
    })
    // This variable holds the frequent quadruples
    val freq4 = count4.filter(kv => kv._2 >= 5).keySet
    println("Found " + freq4.size + " frequent quadruples")

    // Now the pentadruples.  Same basic pattern except we exclude those baskets
    // with fewer than 5 items and we check possible quadruples against frequent
    // ones.
    val count5: java.util.HashMap[(Int, Int, Int, Int, Int), Int] = new java.util.HashMap[(Int, Int, Int, Int, Int), Int]()
    baskets.foreach(basket => {
      val ib = basket.intersect(freq1)
      if (ib.size >= 5) {
        ib.combinations(5).foreach(p => {
          val key: Tuple5[Int,Int,Int,Int,Int] = Tuple5(p(0), p(1), p(2), p(3), p(4))
          if (p.combinations(4).forall(q => freq4.contains((q(0), q(1), q(2), q(3))))) {
            if (count5.keySet.contains(key))
              count5(key) += 1
            else
              count5 += key -> 1
          }
        })
      }
    })
    // This variable holds the frequent pentadruples
    val freq5 = count5.filter(kv => kv._2 >= 5).keySet
    println("Found " + freq5.size + " frequent quintuples")

    // Now the sextuples.  Same basic pattern except we exclude those baskets
    // with fewer than 6 items and we check possible pentadruples against frequent
    // ones.
    val count6: java.util.HashMap[(Int, Int, Int, Int, Int, Int), Int] = new java.util.HashMap[(Int, Int, Int, Int, Int, Int), Int]()
    baskets.foreach(basket => {
      val ib = basket.intersect(freq1)
      if (ib.size >= 6) {
        ib.combinations(6).foreach(s => {
          val key: Tuple6[Int,Int,Int,Int,Int,Int] = Tuple6(s(0), s(1), s(2), s(3), s(4), s(5))
          if (s.combinations(5).forall(p => freq5.contains((p(0), p(1), p(2), p(3), p(4))))) {
            if (count6.keySet.contains(key))
              count6(key) += 1
            else
              count6 += key -> 1
          }
        })
      }
    })
    // This variable holds the frequent sextuples
    val freq6 = count6.filter(kv => kv._2 >= 5).keySet
    println("Found " + freq6.size + " frequent sextuples")

    // Now the heptuples.  Same basic pattern except we exclude those baskets
    // with fewer than 7 items and we check possible sextuples against frequent
    // ones.
    val count7: java.util.HashMap[(Int, Int, Int, Int, Int, Int, Int), Int] = new java.util.HashMap[(Int, Int, Int, Int, Int, Int, Int), Int]()
    baskets.foreach(basket => {
      val ib = basket.intersect(freq1)
      if (ib.size >= 7) {
        ib.combinations(7).foreach(h => {
          val key: Tuple7[Int,Int,Int,Int,Int,Int,Int] = Tuple7(h(0), h(1), h(2), h(3), h(4), h(5), h(6))
          if (h.combinations(6).forall(s => freq6.contains((s(0), s(1), s(2), s(3), s(4), s(5))))) {
            if (count7.keySet.contains(key))
              count7(key) += 1
            else
              count7 += key -> 1
          }
        })
      }
    })
    // This variable holds the frequent heptuples
    val freq7 = count7.filter(kv => kv._2 >= 5).keySet
    println("Found " + freq7.size + " frequent heptuples")

    // Now the octuples.  Same basic pattern except we exclude those baskets
    // with fewer than 7 items and we check possible heptuples against frequent
    // ones.
    val count8: java.util.HashMap[(Int, Int, Int, Int, Int, Int, Int, Int), Int] = new java.util.HashMap[(Int, Int, Int, Int, Int, Int, Int, Int), Int]()
    baskets.foreach(basket => {
      val ib = basket.intersect(freq1)
      if (ib.size >= 8) {
        ib.combinations(8).foreach(o => {
          val key: Tuple8[Int,Int,Int,Int,Int,Int,Int,Int] = Tuple8(o(0), o(1), o(2), o(3), o(4), o(5), o(6), o(7))
          if (o.combinations(7).forall(h => freq7.contains((h(0), h(1), h(2), h(3), h(4), h(5), h(6))))) {
            if (count8.keySet.contains(key))
              count8(key) += 1
            else
              count8 += key -> 1
          }
        })
      }
    })
    // This variable holds the frequent octuples
    val freq8 = count8.filter(kv => kv._2 >= 5).keySet
    println("Found " + freq8.size + " frequent octuples")

    // Now the nonuples.  Same basic pattern except we exclude those baskets
    // with fewer than 7 items and we check possible octuples against frequent
    // ones.
    val count9: java.util.HashMap[(Int, Int, Int, Int, Int, Int, Int, Int, Int), Int] = new java.util.HashMap[(Int, Int, Int, Int, Int, Int, Int, Int, Int), Int]()
    baskets.foreach(basket => {
      val ib = basket.intersect(freq1)
      if (ib.size >= 9) {
        ib.combinations(9).foreach(n => {
          val key: Tuple9[Int,Int,Int,Int,Int,Int,Int,Int,Int] = Tuple9(n(0), n(1), n(2), n(3), n(4), n(5), n(6), n(7), n(8))
          if (n.combinations(8).forall(o => freq8.contains((o(0), o(1), o(2), o(3), o(4), o(5), o(6), o(7))))) {
            if (count9.keySet.contains(key))
              count9(key) += 1
            else
              count9 += key -> 1
          }
        })
      }
    })
    // This variable holds the frequent octuples
    val freq9 = count9.filter(kv => kv._2 >= 5).keySet
    println("Found " + freq9.size + " frequent nonuples")

    // Since there are no frequent nonuples, there can be no frequent itemsets
    // of any larger size.  So, we have a little work to do to calculate all the
    // maximal itemsets.  The complete set is all the octuples, plus any of the
    // frequent itemsets of lesser order whose members are not subsets of any of
    // the frequent itemsets of higher order.
    // First append all the octuples, since we know they all are maximal because
    // there are no nonuples
    val maximals: Buffer[Product] = ArrayBuffer[Product]()
    maximals.appendAll(freq8)

    // Now look back and see which heptuples are maximal and add them.  We do 
    // this by going through each of the frequent octuples and checking all of
    // frequent octuples to see which heptuples are not subsets of any of the
    // frequent octuples.  If we find one, its maximal, so we keep it.
    freq7.foreach(h => {
      if (!freq8.exists(o => Set(h._1, h._2, h._3, h._4, h._5, h._6, h._7).subsetOf(Set(o._1, o._2, o._3, o._4, o._5, o._6, o._7, o._8))))
        maximals.append(h)
    })

    // Now the sextuples
    freq6.foreach(s => {
      if (!freq7.exists(h => Set(s._1, s._2, s._3, s._4, s._5, s._6).subsetOf(Set(h._1, h._2, h._3, h._4, h._5, h._6, h._7))))
        maximals.append(s)
    })

    // Now the pentuples
    freq5.foreach(p => {
      if (!freq6.exists(s => Set(p._1, p._2, p._3, p._4, p._5).subsetOf(Set(s._1, s._2, s._3, s._4, s._5, s._6))))
        maximals.append(p)
    })

    // Now the quadruples
    freq4.foreach(q => {
      if (!freq5.exists(p => Set(q._1, q._2, q._3, q._4).subsetOf(Set(p._1, p._2, p._3, p._4, p._5))))
        maximals.append(q)
    })

    // Now the triples.
    freq3.foreach(t => {
      if (!freq4.exists(q => Set(t._1, t._2, t._3).subsetOf(Set(q._1, q._2, q._3, q._4))))
        maximals.append(t)
    })

    // Now the doubles.
    freq2.foreach(d => {
      if (!freq3.exists(t => Set(d._1, d._2).subsetOf(Set(t._1, t._2, t._3))))
        maximals.append(d)
    })

    // Finally the singles
    freq1.foreach(s => {
      if (!freq2.exists(d => Set(s).subsetOf(Set(d._1, d._2))))
        maximals.append(Tuple1[Int](s))
    })

    // And, finally, output all maximal itemsets
    println("Found " + maximals.size + " Maximal itemsets for this data:")
    maximals.foreach(println)
  }
}