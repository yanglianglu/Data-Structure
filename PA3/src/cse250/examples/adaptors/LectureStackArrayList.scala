/**
 * cse250.examples.adaptors.LectureStackArrayList.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.examples.adaptors

import scala.reflect.ClassTag

class LectureStackArrayList[A: ClassTag]
  extends cse250.examples.types.mutable.StackADT[A] {
  private val _store = new cse250.examples.list.LectureArrayList[A]

  override def push(elem: A): Unit = _store.insert(_store.length, elem)

  override def top: A = _store(_store.length - 1)

  override def pop: A = _store.remove(_store.length - 1)

  override def isEmpty: Boolean = _store.length == 0
}
