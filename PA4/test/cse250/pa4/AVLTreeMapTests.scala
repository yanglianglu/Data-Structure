/**
 * cse250.pa4.AVLTreeMapTests.scala
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


class AVLTreeMapTests extends FlatSpec {
  val testSize = 10
  val inputKeys = Array.tabulate(testSize)(i => i + 1)
  val inputValues = Array.tabulate(testSize)(i => i.toString * i)
  behavior of "AVLTreeMap.insert"
  it should "add the (key,value) pairs" in {
    val treeMap = new AVLTreeMap[Int, String]
    val elements = inputKeys.zip(inputValues)
    for ((k, v) <- elements) {
      treeMap.addOne((k, v))
      assert(treeMap.contains(k))
    }

    val iterator = treeMap.iterator
    for (i <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      assert(elem == elements(i))
      print(elements(i) + " ,")
    }
  }
  behavior of "AVLTreeMap.insert_1"
  it should "add the (key,value) pairs" in {
    val tree = new AVLTree[Int,Int]
    assert(tree.insert(9,9)._value._1 == 9)
    assert(tree._avlRoot._value._1 == 9 && tree.state(tree._avlRoot))
    assert(tree.insert(8,8)._value._1 == 8)
    assert(tree._avlRoot._value._1 == 9 && tree.state(tree._avlRoot,0))
    assert(tree._avlRoot._left._value._1 == 8 && tree.state(tree._avlRoot._left))
      assert(tree.insert(7,7)._value._1 == 7)
    assert(tree._avlRoot._value._1 == 8 && tree.state(tree._avlRoot))
    assert(tree._avlRoot._left._value._1 == 7 && tree.state(tree._avlRoot._left))
    assert(tree._avlRoot._right._value._1 == 9 && tree.state(tree._avlRoot._right))
      assert(tree.insert(6,6)._value._1 == 6)
    assert(tree._avlRoot._value._1 == 8 && tree.state(tree._avlRoot,0))
    assert(tree._avlRoot._left._value._1 == 7 && tree.state(tree._avlRoot._left,0))
    assert(tree._avlRoot._right._value._1 == 9 && tree.state(tree._avlRoot._right))
    assert(tree._avlRoot._left._left._value._1 == 6 && tree.state(tree._avlRoot._left._left))
      assert(tree.insert(5,5)._value._1 == 5)
    assert(tree._avlRoot._value._1 == 8 && tree.state(tree._avlRoot,0))
    assert(tree._avlRoot._left._value._1 == 6 && tree.state(tree._avlRoot._left))
    assert(tree._avlRoot._right._value._1 == 9 && tree.state(tree._avlRoot._right))
    assert(tree._avlRoot._left._left._value._1 == 5 && tree.state(tree._avlRoot._left._left))
    assert(tree._avlRoot._left._right._value._1 == 7 && tree.state(tree._avlRoot._left._right))

      assert(tree.insert(4,4)._value._1 == 4)
    assert(tree._avlRoot._value._1 == 6 && tree.state(tree._avlRoot))
    assert(tree._avlRoot._left._value._1 == 5 && tree.state(tree._avlRoot._left,0))
    assert(tree._avlRoot._right._value._1 == 8 && tree.state(tree._avlRoot._right))
    assert(tree._avlRoot._left._left._value._1 == 4 && tree.state(tree._avlRoot._left._left))
    assert(tree._avlRoot._left._right == null)
    assert(tree._avlRoot._right._left._value._1 == 7 && tree.state(tree._avlRoot._right._left))
    assert(tree._avlRoot._right._right._value._1 == 9 && tree.state(tree._avlRoot._right._right))

      assert(tree.insert(3,3)._value._1 == 3)
    assert(tree._avlRoot._value._1 == 6 && tree.state(tree._avlRoot))
    assert(tree._avlRoot._left._value._1 == 4 && tree.state(tree._avlRoot._left))
    assert(tree._avlRoot._right._value._1 == 8 && tree.state(tree._avlRoot._right))
    assert(tree._avlRoot._left._left._value._1 == 3 && tree.state(tree._avlRoot._left._left))
    assert(tree._avlRoot._left._right._value._1 == 5 && tree.state(tree._avlRoot._left._right))
    assert(tree._avlRoot._right._left._value._1 == 7 && tree.state(tree._avlRoot._right._left))
    assert(tree._avlRoot._right._right._value._1 == 9 && tree.state(tree._avlRoot._right._right))




  }
  behavior of "AVLTreeMap.insert_2"
  it should "add the (key,value) pairs" in {
    val tree = new AVLTree[Int,Int]
    assert(tree.insert(1,1)._value._1 == 1)
    assert(tree._avlRoot._value._1 == 1 && tree.state(tree._avlRoot))

      assert(tree.insert(2,2)._value._1 == 2)
    assert(tree._avlRoot._value._1 == 1 && tree.state(tree._avlRoot,1))
    assert(tree._avlRoot._right._value._1 == 2 && tree.state(tree._avlRoot._right))

      assert(tree.insert(3,3)._value._1 == 3)
    assert(tree._avlRoot._value._1 == 2 && tree.state(tree._avlRoot))
    assert(tree._avlRoot._left._value._1 == 1 && tree.state(tree._avlRoot._left))
    assert(tree._avlRoot._right._value._1 == 3 && tree.state(tree._avlRoot._right))

      assert(tree.insert(4,4)._value._1 == 4)
    assert(tree._avlRoot._value._1 == 2 && tree.state(tree._avlRoot,1))
    assert(tree._avlRoot._left._value._1 == 1 && tree.state(tree._avlRoot._left))
    assert(tree._avlRoot._right._value._1 == 3 && tree.state(tree._avlRoot._right,1))
    assert(tree._avlRoot._right._right._value._1 == 4 && tree.state(tree._avlRoot._right._right))

      assert(tree.insert(5,5)._value._1 == 5)
    assert(tree._avlRoot._value._1 == 2 && tree.state(tree._avlRoot,1))
    assert(tree._avlRoot._left._value._1 == 1 && tree.state(tree._avlRoot._left))
    assert(tree._avlRoot._right._value._1 == 4 && tree.state(tree._avlRoot._right))
    assert(tree._avlRoot._right._left._value._1 == 3 && tree.state(tree._avlRoot._right._left))
    assert(tree._avlRoot._right._right._value._1 == 5 && tree.state(tree._avlRoot._right._right))

      assert(tree.insert(6,6)._value._1 == 6)
    assert(tree._avlRoot._value._1 == 4 && tree.state(tree._avlRoot))
    assert(tree._avlRoot._left._value._1 == 2 && tree.state(tree._avlRoot._left))
    assert(tree._avlRoot._right._value._1 == 5 && tree.state(tree._avlRoot._right,1))
    assert(tree._avlRoot._left._left._value._1 == 1 && tree.state(tree._avlRoot._left._left))
    assert(tree._avlRoot._left._right._value._1 == 3 && tree.state(tree._avlRoot._left._right))
    assert(tree._avlRoot._right._left == null)
    assert(tree._avlRoot._right._right._value._1 == 6 && tree.state(tree._avlRoot._right._right))

      assert(tree.insert(7,7)._value._1 == 7)
    assert(tree._avlRoot._value._1 == 4 && tree.state(tree._avlRoot))
    assert(tree._avlRoot._left._value._1 == 2 && tree.state(tree._avlRoot._left))
    assert(tree._avlRoot._right._value._1 == 6 && tree.state(tree._avlRoot._right))
    assert(tree._avlRoot._left._left._value._1 == 1 && tree.state(tree._avlRoot._left._left))
    assert(tree._avlRoot._left._right._value._1 == 3 && tree.state(tree._avlRoot._left._right))
    assert(tree._avlRoot._right._left._value._1 == 5 && tree.state(tree._avlRoot._right._left))
    assert(tree._avlRoot._right._right._value._1 == 7 && tree.state(tree._avlRoot._right._right))

  }
  behavior of "AVLTreeMap.insert_3"
  it should "add the (key,value) pairs" in {
    val tree = new AVLTree[Int,Int]
    assert(tree.insert(9,9)._value._1 == 9)
    assert(tree._avlRoot._value._1 == 9 && tree.state(tree._avlRoot))
      assert(tree.insert(7,7)._value._1 == 7)
    assert(tree._avlRoot._value._1 == 9 && tree.state(tree._avlRoot,0))
    assert(tree._avlRoot._left._value._1 == 7 && tree.state(tree._avlRoot._left))
      assert(tree.insert(6,6)._value._1 == 6)
    assert(tree._avlRoot._value._1 == 6 && tree.state(tree._avlRoot))
    assert(tree._avlRoot._left._value._1 == 7 && tree.state(tree._avlRoot._left))
    assert(tree._avlRoot._right._value._1 == 9 && tree.state(tree._avlRoot._right))
      assert(tree.insert(1,1)._value._1 == 1)
    assert(tree._avlRoot._value._1 == 6 && tree.state(tree._avlRoot,0))
    assert(tree._avlRoot._left._value._1 == 7 && tree.state(tree._avlRoot._left,0))
    assert(tree._avlRoot._right._value._1 == 9 && tree.state(tree._avlRoot._right))
    assert(tree._avlRoot._left._left._value._1 == 1 && tree.state(tree._avlRoot._left._left))
      assert(tree.insert(5,5)._value._1 == 5)
    assert(tree._avlRoot._value._1 == 6 && tree.state(tree._avlRoot,0))
    assert(tree._avlRoot._left._value._1 == 5 && tree.state(tree._avlRoot._left))
    assert(tree._avlRoot._right._value._1 == 9 && tree.state(tree._avlRoot._right))
    assert(tree._avlRoot._left._left._value._1 == 1 && tree.state(tree._avlRoot._left._left))
    assert(tree._avlRoot._left._right._value._1 == 7 && tree.state(tree._avlRoot._left._right))

      assert(tree.insert(3,3)._value._1 == 3)
    assert(tree._avlRoot._value._1 == 5 && tree.state(tree._avlRoot))
    assert(tree._avlRoot._left._value._1 == 1 && tree.state(tree._avlRoot._left,1))
    assert(tree._avlRoot._right._value._1 == 6 && tree.state(tree._avlRoot._right))
    assert(tree._avlRoot._left._left == null)
    assert(tree._avlRoot._left._right._value._1 == 3 && tree.state(tree._avlRoot._left._right))
    assert(tree._avlRoot._right._left._value._1 == 7 && tree.state(tree._avlRoot._right._left))
    assert(tree._avlRoot._right._right._value._1 == 9 && tree.state(tree._avlRoot._right._right))

      assert(tree.insert(2,2)._value._1 == 2)
    assert(tree._avlRoot._value._1 == 5 && tree.state(tree._avlRoot))
    assert(tree._avlRoot._left._value._1 == 2 && tree.state(tree._avlRoot._left))
    assert(tree._avlRoot._right._value._1 == 6 && tree.state(tree._avlRoot._right))
    assert(tree._avlRoot._left._left._value._1 == 1 && tree.state(tree._avlRoot._left._left))
    assert(tree._avlRoot._left._right._value._1 == 3 && tree.state(tree._avlRoot._left._right))
    assert(tree._avlRoot._right._left._value._1 == 7 && tree.state(tree._avlRoot._right._left))
    assert(tree._avlRoot._right._right._value._1 == 9 && tree.state(tree._avlRoot._right._right))

  }
  behavior of "AVLTreeMap.remove"
  it should "remove the (key,value) pairs" in {
    val tree = new AVLTree[Int,Int]()
    for(i <- 0 to 8)
      tree.insert((i,i))
    val remove_9 = tree.remove(9)
    val iterator_9 = tree.iterator
    assert(!remove_9)
    for(i <- 0 to 8){
      assert(iterator_9.next()._1 == i)
    }

    val remove_7 = tree.remove(7)
    val iterator_7 = tree.iterator
    assert(remove_7)
    for(i <- 0 to 6){
      assert(iterator_7.next()._1 == i)
    }
    assert(iterator_7.next()._1 == 8)


    val remove_0 = tree.remove(0)
    val iterator_0 = tree.iterator
    assert(remove_0)
    for(i <- 1 to 6){
      assert(iterator_0.next()._1 == i)
    }
    assert(iterator_0.next()._1 == 8)
    assertThrows[NoSuchElementException](iterator_0.next())

    val remove_1 = tree.remove(1)
    val iterator_1 = tree.iterator
    assert(remove_1)
    for(i <- 2 to 6){
      assert(iterator_1.next()._1 == i)
    }
    assert(iterator_1.next()._1 == 8)
    assertThrows[NoSuchElementException](iterator_1.next())
    tree.remove(6)
    tree.remove(4)
    tree.remove(8)
  }
  behavior of "AVLTreeMap.update"
  it should "update the (key,value) pairs" in {
    val tree = new AVLTree[Int,Int]()
    tree.insert((1,1))
    tree.insert((1,1))
    tree.insert((2,2))

    for(i <- 0 to 12){
      tree.insert((i,i))
    }
    for(i <- 0 to 12)
      tree.insert((i,i+1))

    val iterable = tree.iterator
    var i = 0
    while(iterable.hasNext){
      assert(iterable.next() == (i,i+1))
      i+=1
    }
  }
  behavior of "insert_test"
  it should "insert correctly" in {
    val tree = new AVLTree[Int,Int]()
    for(i <- 9999 to 0 by -1) {
      val j = (Math.random() * 10000).toInt
      print(j + ", ")
      tree.insert((j,j))
      tree.testAVL(tree._avlRoot)

    }

  }
}

