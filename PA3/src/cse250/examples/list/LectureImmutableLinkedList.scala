/**
 * cse250.examples.list.LectureImmutableLinkedList.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.examples.list

sealed trait LectureImmutableLinkedList[+A] extends collection.immutable.Seq[A]
  with cse250.examples.types.immutable.ListADT[A] {

  override def head: A = this match {
    case n: ListNode[A] => n._value
    case EmptyList => throw new IllegalArgumentException("EmptyList has not head value.")
  }

  override def tail: LectureImmutableLinkedList[A] = this match {
    case n: ListNode[A] => n._next
    case EmptyList => throw new IllegalArgumentException("EmptyList has not tail reference.")
  }

  override def length: Int = this match {
    case _: ListNode[A] => 1 + this.tail.length
    case EmptyList => 0
  }

  override def isEmpty = this match {
    case _: ListNode[A] => false
    case EmptyList => true
  }

  override def apply(idx: Int): A = {
    if (idx == 0) head
    else tail(idx - 1)
  }

  override def updated[B >: A](idx: Int, elem: B): LectureImmutableLinkedList[B] = {
    if (idx == 0) ListNode[B](elem, tail)
    else ListNode[B](head, tail.updated(idx - 1, elem))
  }

  override def inserted[B >: A](idx: Int, elem: B): LectureImmutableLinkedList[B] = {
    if (idx == 0) ListNode[B](elem, this)
    else ListNode[B](head, tail.inserted(idx - 1, elem))
  }

  override def removed(idx: Int): LectureImmutableLinkedList[A] = {
    if (idx == 0) tail
    else ListNode[A](head, tail.removed(idx - 1))
  }

  override def prepended[B >: A](elem: B): LectureImmutableLinkedList[B] = ListNode[B](elem,this)

  def ::[B >: A](elem: B): LectureImmutableLinkedList[B] = ListNode[B](elem,this)

  override def iterator: Iterator[A] = new Iterator[A] {
    var _currentList = LectureImmutableLinkedList.this

    override def hasNext: Boolean = !_currentList.isEmpty

    override def next: A = {
      val retval = _currentList.head
      _currentList = _currentList.tail
      retval
    }
  }
}

case class ListNode[A](_value: A, _next: LectureImmutableLinkedList[A]) extends LectureImmutableLinkedList[A]

object EmptyList extends LectureImmutableLinkedList[Nothing]