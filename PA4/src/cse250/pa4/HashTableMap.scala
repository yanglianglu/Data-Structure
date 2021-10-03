/**
 * cse250.pa5.HashTableMap.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:yanglian
 * Person#:50258239
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa4

import cse250.examples.types.mutable.Map

import scala.collection.IterableOnce
import scala.collection.mutable.ListBuffer
import scala.util.hashing.Hashing

class HashTableMap[K, V](_alphaMax: Double = 0.6)(implicit hash: Hashing[K]) extends Map[K, V] {
  var _n = 0
  var _N = 10
  var _alpha: Double = 0.0
  var _bucketArray = Array.fill[ListBuffer[(K, V)]](_N)(ListBuffer[(K, V)]())

  def rehash(newSize: Int): Unit = {
    if (newSize > _N) {
      val oldBucketArray = _bucketArray
      _n = 0
      _N = newSize
      _alpha = 0.0
      _bucketArray = Array.fill(_N)(ListBuffer[(K, V)]())
      for (bucket <- oldBucketArray; elem <- bucket) addOne(elem)
    }
  }

  override def get(key: K): Option[V] = {
    val lookupIndex = hash.hash(key) % _N
    _bucketArray(lookupIndex).find(elem => elem._1 == key) match {
      case Some(elem) => Some(elem._2)
      case None       => None
    }
  }

  override def addOne(elem: (K, V)): Unit = {
    val index = hash.hash(elem._1) % _N
   if(_alpha < _alphaMax){
     val exist = get(elem._1).isEmpty
     if(exist){
       _bucketArray(index) = _bucketArray(index).prepended(elem)
       _n += 1
       _alpha = _n.toDouble / _N
     }
     else{
       var idx = 0
       for(i <- 0 to  _bucketArray(index).length -1){
         if(_bucketArray(index)(i)._1 == elem._1)
           idx = i;
       }
       _bucketArray(index) = _bucketArray(index).updated(idx,elem)
     }
   }
    else{
     rehash(_N*2)
     addOne(elem)
   }
  }

  override def removeOne(key: K): Boolean = {
    val lookupIndex = hash.hash(key) % _N
    _bucketArray(lookupIndex).find(elem => elem._1 == key) match {
      case Some(elem) => {
        _bucketArray(lookupIndex).remove(_bucketArray(lookupIndex).indexOf(elem))
        _n = _n - 1
        _alpha = _n.toDouble/_N
        return true
      }
      case None       => false
    }
  }

  override def iterator: Iterator[(K, V)] = new Iterator[(K, V)]{
    var iterable : List[(K,V)] = List()
    for (bucket <- _bucketArray; elem <- bucket){
      iterable = iterable.appended(elem)
    }
    val len = iterable.length
    var nlen = 0
    override def hasNext: Boolean = nlen < len

    override def next(): (K, V) = {
      val v = iterable(nlen)
      nlen += 1
      v
    }

  }
}
