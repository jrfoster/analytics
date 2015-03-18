package org.jrfoster.datamining

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf
import org.apache.spark.rdd.RDD
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer

object DocumentWordCount {

  def main(args: Array[String]) = {
    val conf = new SparkConf().setAppName("wordcounter").setMaster("local[1]")
    val sc = new SparkContext(conf)

    val b1 = sc.parallelize(List("Cat", "and", "dog", "bites"))
    val b2 = sc.parallelize(List("Yahoo", "news", "claims", "a", "cat", "mated", "with", "a", "dog", "and", "produced", "viable", "offspring"))
    val b3 = sc.parallelize(List("Cat", "killer", "likely", "is", "a", "big", "dog"))
    val b4 = sc.parallelize(List("Professional", "free", "advice", "on", "dog", "training", "puppy", "training"))
    val b5 = sc.parallelize(List("Cat", "and", "kitten", "training", "and", "behavior"))
    val b6 = sc.parallelize(List("Dog", "&", "Cat", "provides", "dog", "training", "in", "Eugene", "Oregon"))
    val b7 = sc.parallelize(List("“Dog", "and", "cat”", "is", "a", "slang", "term", "used", "by", "police", "officers", "for", "a", "male–female", "relationship"))
    val b8 = sc.parallelize(List("Shop", "for", "your", "show", "dog", "grooming", "and", "pet", "supplies"))

    val allWords = b1.union(b2).union(b3).union(b4).union(b5).union(b6).union(b7).union(b8)
    
    // Use a standard word counting mechanism with threshold = 3 to produce a
    // list of frequent singletons
    val freqSingletons = allWords
      .flatMap(line => line.toLowerCase().split(" "))
      .map(word => (word, 1))
      .reduceByKey(_ + _)
      .filter(t => t._2 > 3)
      .map(t => t._1)

    // Print out all our frequent singletons
    println("Singletons with threshold > 3")
    freqSingletons.foreach(println)
    
    val test:ListBuffer[(String, String)] = new ListBuffer()
    
    // Here we create a list of all doubletons where both items in the pair are
    // different (meaning we remove sets like {a,a} or {dog,dog}
    val allDoubles = freqSingletons
      .cartesian(freqSingletons)
      .filter(t => t._1 != t._2)
      .aggregate(ListBuffer[(String,String)]()) ((r, v) => {
        println(r.contains(("a","training")))
        if (r.contains(v) || r.contains(v.swap)) {
          r
        } else {
          r+=(v)
        }}, 
        (r,v) => {r.++(v)})
      
    allDoubles.foreach(println)

  }

  def func(v1:(String,String), v2:(String, String)) : Boolean = {
    val s1 = Seq(v1._1, v1._2).sorted
    val s2 = Seq(v2._1, v2._2).sorted
    val rv = v1 == v2
    return rv
  }
  
  
}

