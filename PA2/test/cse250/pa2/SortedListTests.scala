/**
 * cse250.pa2.SortedListTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
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
package cse250.pa2

import cse250.adaptors.LectureQueue
import org.scalatest.{BeforeAndAfter, FlatSpec}
import org.scalatest.Assertions._


class SortedListTests extends FlatSpec with BeforeAndAfter {
  behavior of "insert"
  it should "insert a solo element into list at index 0" in {
    val myList = new SortedList[Int]
    val valToInsert = 5
    myList.insert(valToInsert)
    assert(myList.length == 1)
    assert(myList(0) == valToInsert)
  }

  behavior of "processBatch"
  it should "process two insertions" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",0)
    jobQueue.enqueue("insert",0)
    myList.processBatch(jobQueue)
    // Should have inserted the values: 0,0.
    assert(myList.length == 2)
    assert(myList(0) == 0)
    assert(myList(1) == 0)
    // Should have removed both copies of 0.
    jobQueue.enqueue("remove",0)
    myList.processBatch(jobQueue)
    assert(myList.length == 0)
  }
  behavior of "apply"
  it should "return value in that given index" in {
    val myList = new SortedList[Int]
    assertThrows[IllegalArgumentException](myList(0))
    myList.insert(1)
    assert(myList(0) == 1)
    myList.insert(2)
    myList.insert(1)
    assert(myList(2) == 2)
    assert(myList(1) == 1)
  }

  behavior of "length"
  it should "return the length og the list" in {
    val myList = new SortedList[Int]
    assert(myList.length == 0)
    myList.insert(1)
    for (i <- 1 to 9) {
      myList.insert(1)
      assert(myList.length == (i + 1))
    }
  }
  behavior of "iterator"
  it should "loop n times as n == length" in {
    val myList = new SortedList[Long]
    var loop = 0
    while(myList.iterator.hasNext){
      loop += 1;
    }
    assert(loop == 0)

    myList.insert(1)
    val iterator = myList.iterator
    while(iterator.hasNext) {
      loop += 1
      iterator.next()
    }
    assert(loop == 1)
    loop = 0

    myList.remove(1)
    for(i <- 0 to 900) {
      myList.insert(math.round(math.random()*10))
    }
    val iterator_1 = myList.iterator
    while(iterator_1.hasNext) {
      loop += 1
      iterator_1.next()
    }
    assert(loop == 901)
  }
behavior of "insert"
  it should "insert a value within the order" in{
    val myList = new SortedList[Int]
    myList.insert(1)
    assert(myList(0) == 1)
    myList.insert(0)
    assert(myList(0) == 0 && myList(1) == 1)
    myList.remove(0)
    myList.remove(1)
    assert(myList.length == 0)
    myList.insert(3)
    myList.insert(1)
    assert(myList(0) == 1)
    assert(myList(1) == 3)
    myList.insert(4)
    assert(myList(2) == 4)
    myList.insert(2)
    for(i <- 0 to 3)
      assert(myList(i) == i+1 )
    myList.insert(4)
    assert(myList(3) == 4 && myList(4) == 4)
  }

behavior of "remove"
  it should "remove all n with a given n" in{
    def exist(myList : SortedList[Int], value : Int): Unit ={
      val iterator = myList.iterator
      while(iterator.hasNext){
        val num = iterator.next()
        assert(num != value)
      }
    }
    val myList = new SortedList[Int]
    assert(myList.remove(1) == false)
    myList.insert(2)
    assert(myList.remove(1) == false)
    myList.insert(1)
    assert(myList.remove(1) == true)
    assert(myList(0) == 2)
    exist(myList,1)
    for(i <- 0 to 100){
      myList.insert(1)
    }
    assert(myList.remove(1) == true)
    exist(myList,1)

    for(i <- 0 to 100){
      myList.insert(i)
    }
    myList.insert(10)
    assert(myList.remove(10) == true)
    exist(myList,10)
  }
behavior of "processBatch"
  it should "process the jobs in order" in{
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    for(i <- 0 to 9)
      jobQueue.enqueue("insert",i)
    myList.processBatch(jobQueue)
    for( i <- 0 to 9)
      assert(myList.length == 10 && myList(i) == i)
    for(i <- 0 to 9)
      jobQueue.enqueue("remove",i)
    myList.processBatch(jobQueue)
      assert(myList.length == 0)

  }
behavior of "undo"
  it should "undo the last change" in{
    val myList = new SortedList[Int]
    assertThrows[IllegalArgumentException](myList.undoLastModification())
    myList.insert(1)
    myList.undoLastModification()
    assert(myList.length == 0)
    myList.insert(1)
    myList.insert(1)
    myList.undoLastModification()
    assert(myList.length == 1 && myList(0) == 1)
    myList.undoLastModification()
    assert(myList.length == 0)
    assertThrows[IllegalArgumentException](myList.undoLastModification())

    for(i <- 0 to 9)
      myList.insert(i)
      myList.insert(9)
      myList.remove(9)
      myList.undoLastModification()
      assert(myList(9) == 9 && myList(10) == 9)

      myList.undoLastModification()
      for(i <- 0 to 9){
        assert(myList.length == 10 - i && myList(9 - i) == 9 - i)
        myList.undoLastModification()
      }
    assertThrows[IllegalArgumentException](myList.undoLastModification())
    assert(myList.length == 0)

    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",2)
    jobQueue.enqueue("remove",1)
    jobQueue.enqueue("insert",1)
    myList.processBatch(jobQueue)
    myList.undoLastModification()
    assert(myList.length == 0)
    assertThrows[IllegalArgumentException](myList.undoLastModification())

    myList.insert(3)
    jobQueue.enqueue("insert",2)
    jobQueue.enqueue("remove",1)
    jobQueue.enqueue("insert",1)
    jobQueue.enqueue("insert",2)
    jobQueue.enqueue("insert",1)
    myList.processBatch(jobQueue)
    myList.undoLastModification()
    assert(myList(0) == 3, myList.length == 1)
    myList.undoLastModification()
    assert(myList.length == 0)
    assertThrows[IllegalArgumentException](myList.undoLastModification())

    myList.insert(0)
    jobQueue.enqueue("insert",2)
    jobQueue.enqueue("remove",1)
    jobQueue.enqueue("insert",1)
    jobQueue.enqueue("insert",2)
    jobQueue.enqueue("insert",1)
    jobQueue.enqueue("remove",2)
    jobQueue.enqueue("remove",1)
    myList.processBatch(jobQueue)
    assert(myList.length == 1)
    myList.undoLastModification()
    assert(myList.length == 0)
    assertThrows[IllegalArgumentException](myList.undoLastModification())

    jobQueue.enqueue("insert",2)
    jobQueue.enqueue("remove",1)
    jobQueue.enqueue("insert",1)
    jobQueue.enqueue("insert",2)
    jobQueue.enqueue("insert",1)
    jobQueue.enqueue("remove",5)
    jobQueue.enqueue("remove",4)
    myList.processBatch(jobQueue)
    myList.insert(5)
    myList.insert(6)
    myList.remove(2)
    myList.undoLastModification()
    assert(myList.length == 6 && myList(2) == myList(3) && myList(2) == 2)
    myList.remove(1)
    myList.remove(5)
    jobQueue.enqueue("insert",2)
    jobQueue.enqueue("remove",1)
    jobQueue.enqueue("insert",1)
    jobQueue.enqueue("insert",2)
    jobQueue.enqueue("insert",1)
    jobQueue.enqueue("remove",5)
    jobQueue.enqueue("remove",4)
    myList.processBatch(jobQueue)
    myList.undoLastModification()
    myList.undoLastModification()
    myList.undoLastModification()
    assert(myList.length == 6 && myList(2) == myList(3) && myList(2) == 2)



  }
}
