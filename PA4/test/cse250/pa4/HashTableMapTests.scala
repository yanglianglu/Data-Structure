/**
 * cse250.pa4.HashTableMapTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 */

package cse250.pa4

import org.scalatest.FlatSpec

class HashTableMapTests extends FlatSpec {
  val testSize = 10
  val inputKeys = Array.tabulate(testSize)(i => i + 1)
  val inputValues = Array.tabulate(testSize)(i => i.toString * i)
  behavior of "HashTableMap.insert"
  it should "add the (key,value) pairs" in {
    val hashMap = new HashTableMap[Int, String]
    val elements = inputKeys.zip(inputValues)
    for ((k, v) <- elements) {
      hashMap.addOne((k, v))
      assert(hashMap.contains(k))
    }
    val iterator = hashMap.iterator
    val elementSet = collection.mutable.Set[(Int, String)]()
    for (_ <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      elementSet.add(elem)
    }
    for (i <- elements.indices) {
      val elem = elements(i)
      assert(elementSet.contains(elem))
    }
  }
  behavior of "HashTableMap.remove"
  it should "remove the (key,value) pairs" in{
    val hashTable = new HashTableMap[Int,Int]()
    assert(!hashTable.removeOne(1))
    for(i <- 0 to 40)
      hashTable.addOne((i,i))
    for(i <- 0 to 40)
      hashTable.addOne((i,i+1))
    val iterable = hashTable.iterator
    var j = 0
    while(iterable.hasNext){
      assert(iterable.next() == (j,j+1))
      j += 1
    }
    for(i <- 0 to 40){
      assert(hashTable.removeOne(i))
    }
    val none = hashTable.iterator

    assert(none.length == 0)
  }
}

