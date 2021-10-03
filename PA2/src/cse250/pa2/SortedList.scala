/**
 * cse250.pa2.SortedList.scala
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

import cse250.adaptors.{LectureQueue, LectureStack}

class SortedList[A] (implicit _comp: Ordering[A]) extends collection.Seq[A] {
  // Updates the toString to mention our class name instead of Seq.
  override protected[this] def className = "SortedList"

  // Use _storageList to maintain the sorted list.
  var _storageList: cse250.list.ImmutableLinkedList[A] = cse250.list.EmptyList
  // ---------- MAKE CHANGES BELOW ----------

  val _undo : LectureStack[cse250.list.ImmutableLinkedList[A]] = new LectureStack[cse250.list.ImmutableLinkedList[A]]
  var processing = false
  override def apply(i: Int): A = {
    require(_storageList.length > i || i >= 0)

      _storageList.apply(i)
    
  }

  /** Gets the number of elements stored within the list. */
  override def length: Int = _storageList.length

  /** Returns an Iterator that can be used only once. */
  override def iterator: Iterator[A] = _storageList.iterator

  /**
   * Inserts one copy of elem into the list in non-decreasing order.
   * @param elem element to be inserted.
   */
  def insert(elem: A): Unit = {
    var isInserted : Boolean = false
    if(!processing)
      _undo.push(_storageList)
    if (_storageList.isEmpty) {

      _storageList = _storageList.inserted(0, elem)
      isInserted = true
    }
    else{

      for( i <- _storageList){
        if(_comp.lteq(elem,i) && isInserted == false) {

          _storageList = _storageList.inserted(_storageList.indexOf(i),elem)
          isInserted = true;
        }
      }
      if(!isInserted){

        _storageList = _storageList.inserted(_storageList.length,elem)
      }
    }

  }

  /**
   * Removes all copies of elem from the list.
   * @param elem element to be removed.
   * @return true if any change has been made, and false otherwise.
   */
  def remove(elem: A): Boolean = {
    var isPushed = true
    if(_storageList.indexOf(elem) == -1)
      return false;
    else{
      while(_storageList.indexOf(elem) != -1) {
        if(isPushed) {
          if(!processing)
            _undo.push(_storageList)
          isPushed = false
        }
          _storageList =  _storageList.removed(_storageList.indexOf(elem))
      }

    }
    return true
  }

  /** Takes in a queue of valid operations to perform. Each pair has the form:
   *      (OP,elem)
   *  where:
   *      OP will be the string "insert" or "remove"
   *      elem will be a value of type A to use as the argument to OP. */
  def processBatch(operations: cse250.types.mutable.QueueADT[(String,A)]): Unit = {
    processing = true
    val current : cse250.list.ImmutableLinkedList[A] = _storageList
    while(!operations.isEmpty){
      val node : (String, A) = operations.dequeue
      if(node._1 == "insert") {
          insert(node._2)
      } else if(node._1 == "remove") {
        remove(node._2)

      }
    }
    if(!compare(current,_storageList)) {
      _undo.push(current)
    }
    processing = false
  }

  /** Undo the last modification, if any change has been made.
   * If no change to undo exists, throw an IllegalArgumentException.
   */
  def undoLastModification(): Unit = {
    require(!_undo.isEmpty)
        _storageList = _undo.pop
  }
  def compare(before : cse250.list.ImmutableLinkedList[A], after : cse250.list.ImmutableLinkedList[A]) : Boolean = {
    if(before.length == 0 && after.length == 0) {
      print(1)
        return true
    }
    if(before.length == after.length) {
      for (i <- 0 until before.length) {
        if (!_comp.equiv(before(i), after(i)))
          return false
        }
      }
    else {
      return false
    }
    return true
  }
}
