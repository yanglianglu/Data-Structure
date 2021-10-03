/**
 * cse250.pa5.AVLTreeMap.scala
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

import collection.mutable.Stack
import scala.annotation.tailrec

class AVLTreeMap[K, V]()(implicit ord: Ordering[K]) extends Map[K, V]{
  val _storageTree = new AVLTree[K, V]

  override def addOne(elem: (K, V)): Unit = _storageTree.insert(elem)

  override def removeOne(key: K): Boolean = _storageTree.remove(key)

  override def get(key: K): Option[V] = _storageTree.find(key) match {
    case n: _storageTree.AVLNode[(K, V)] if n != null => Some(n._value._2)
    case null                                         => None
  }

  override def iterator: Iterator[(K, V)] = _storageTree.iterator
}

class AVLTree[K, V]()(implicit ord: Ordering[K]) {

  class AVLNode[A](var _value: A, var _left: AVLNode[A], var _right: AVLNode[A], var _parent: AVLNode[A],
                   var _leftH: Boolean, var _rightH: Boolean)

  var _avlRoot: AVLNode[(K, V)] = null

  def find(elem: K): AVLNode[(K, V)] = {
    var current = _avlRoot
    var found = false
    while (!found && current != null) {
      val currentKey = current._value._1
      if (ord.lt(elem, currentKey)) current = current._left
      else if (ord.lt(currentKey, elem)) current = current._right
      else found = true
    }
    current
  }

  def rotateLeft(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    val x = nodeA
    val y = nodeA._right
    val z = nodeA._right._left

    if(x._parent != null) {
      if (x._parent._left != null && ord.equiv(x._parent._left._value._1, x._value._1))
        x._parent._left = y
      else x._parent._right = y
    }
    y._parent = x._parent
    x._parent = y
    if(z != null)
      z._parent = x

    val node = y
    node._left = x
    node._left._right = z

    return node
  }

  def rotateRight(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    val x = nodeA
    val y = nodeA._left
    val z = nodeA._left._right
    if(x._parent != null) {
      if (x._parent._left != null && ord.equiv(x._parent._left._value._1, x._value._1))
        x._parent._left = y
      else x._parent._right = y
    }
    y._parent = x._parent
    x._parent = y
    if(z != null)
      z._parent = x

    val node = y
    node._right = x
    node._right._left = z

    return node
  }

  def rotateLeftRight(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    nodeA._left = rotateLeft(nodeA._left)
    return rotateRight(nodeA)
  }

  def rotateRightLeft(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    nodeA._right = rotateRight(nodeA._right)
    return rotateLeft(nodeA)
  }

  def insert(elem: (K, V)): AVLNode[(K, V)] = {
    var current = _avlRoot
    var found = false
    if(_avlRoot == null) {
      _avlRoot = new AVLNode(elem,null,null,current,false,false)
      return _avlRoot
    }

    while (!found && current != null) {
      val currentKey = current._value._1
      if (ord.lt(elem._1, currentKey)){
        if(current._left == null) {
          current._left = new AVLNode(elem,null,null,current,false,false)
          found = true
        }
          current = current._left
      }
      else if (ord.lt(currentKey, elem._1)) {
        if(current._right == null){
          current._right = new AVLNode(elem,null,null,current,false,false)
          found = true
        }
          current = current._right

      }
      else {
        current._value = elem
        found = true
        return current
      }
    }
    var exist = false
    var parent = current._parent
    var child = current
    while(!exist){
      val isRoot = ord.equiv(_avlRoot._value._1,parent._value._1)
      var L_O_R : Boolean = true
      if(parent._left == null || !ord.equiv(parent._left._value._1,child._value._1))
        L_O_R = false
      if(parent == _avlRoot)
        exist = true

      if(!parent._rightH && !parent._leftH){


        if(L_O_R){
          parent._leftH = true
        }
        else {
          parent._rightH = true
        }
        parent = parent._parent
        child = child._parent
      }

      else if(parent._leftH){//left heavy


        if(L_O_R){//left child

          if(child._rightH){
            child._rightH = false
            parent._leftH = false

            parent = rotateLeftRight(parent)
            if(isRoot)
              _avlRoot = parent
            exist = true
          }
          else if(child._leftH){
            child._leftH = false
            parent._leftH = false
            parent = rotateRight(parent)
            if(isRoot)
              _avlRoot = parent
            exist = true
          }
          else{
            parent = parent._parent
            child = child._parent
          }
        }

        else {
          parent._leftH = false
        }

      }

      else if(parent._rightH){
        if(!L_O_R){//right child

          if(child._rightH){
            child._rightH = false
            parent._rightH = false
            parent = rotateLeft(parent)
            exist = true
            if(isRoot)
              _avlRoot = parent
          }
          else if(child._leftH){
            child._leftH = false
            parent._rightH = false
            parent = rotateRightLeft(parent)
            exist = true
            if(isRoot)
              _avlRoot = parent
          }
          else{
            parent = parent._parent
            child = child._parent
          }
        }

        else {
          parent._rightH = false
          exist = true
        }
      }

    }



    return current
  }

  def remove(key: K): Boolean = {
    var current = _avlRoot
    var found = false
    while (!found && current != null) {
      val currentKey = current._value._1
      if (ord.lt(key, currentKey)) current = current._left
      else if (ord.lt(currentKey, key)) current = current._right
      else {
        found = true
        removing()
      }
    }
    def removing() : Unit ={
      var search = current
      var parent = search._parent
      if(current._left == null && current._right == null){
        if(ord.equiv(current._value._1,_avlRoot._value._1))
          _avlRoot = null
        else{

          if(parent._left != null && ord.equiv(parent._left._value._1,search._value._1)) {
            connect(search._left,"left")
            fix("left")
          }
          else {
            connect(search._right,"right")

            fix("right")
          }

        }
      }
      else if(current._left == null){
        val L_O_R = left_or_right(parent,search)
        connect(search._right,L_O_R)
        replace(search._right,current)
        parent = search
        fix("right")

      }
      else if(current._right == null){
        val L_O_R = left_or_right(parent,search)
        connect(search._left,L_O_R)
        replace(search._left,current)
        parent = search
        fix("left")
      }
      else{
        search = smallest(current)
        val L_O_R = left_or_right(current._parent,current)
        val search_L_O_R = left_or_right(search._parent,search)
        connect(search,L_O_R)
        if(ord.equiv(search._parent._value._1,current._value._1))
          parent = search
        replace(search,current)

        fix(search_L_O_R)
      }
      def fix(L_O_R : String): Unit ={
        var f_f = false
        if(parent != null && L_O_R != "end"){
          if(L_O_R == "left"){
            if(!parent._leftH){
              if(parent._rightH) {

                if(parent._right._right != null && parent._right._left != null && parent._left == null){
                  parent = rotateLeft(parent)
                  parent._rightH = false
                  parent._leftH = true
                  parent._left._rightH = true
                  parent._left._leftH = false
                }
                else if( parent._right._left == null || parent._left != null) {
                  parent = rotateLeft(parent)
                  parent._rightH = false
                  parent._leftH = false
                  parent._left._rightH = false
                  parent._left._leftH = false
                }
                else{
                  parent = rotateRightLeft(parent)
                  parent._rightH = false
                  parent._leftH = false
                  parent._left._rightH = false
                  parent._left._leftH = false
                }
                if(ord.equiv(parent._left._value._1,_avlRoot._value._1))
                  _avlRoot = parent
                f_f = true
              }
              else {
                parent._rightH = true
                f_f = true
              }
            }
            else{
              parent._leftH = false
            }
          }
          else {
            if(parent._leftH) {

              if(parent._left._left != null && parent._left._right != null && parent._right == null) {
                parent = rotateRight(parent)
                parent._leftH = false
                parent._rightH = true
                parent._right._leftH = true
                parent._right._rightH = false
              }
              else if( parent._left._right == null || parent._right != null) {
                parent = rotateRight(parent)
                parent._leftH = false
                parent._rightH = false
                parent._right._leftH = false
                parent._right._rightH = false
              }
              else{
                parent = rotateLeftRight(parent)
                parent._leftH = false
                parent._rightH = false
                parent._right._leftH = false
                parent._right._rightH = false
              }
              if(ord.equiv(parent._right._value._1,_avlRoot._value._1))
                _avlRoot = parent
              f_f = true

            }
            else {
              if(parent._rightH == false) {
                parent._leftH = true
                f_f = true
              }
              else parent._rightH = false
            }
          }
          if(parent._parent == null || f_f){
            parent = parent._parent
            fix("end")
          }
          else {
            if(parent._parent._left != null && ord.equiv(parent._parent._left._value._1,parent._value._1)){
              parent = parent._parent
              fix("left")
            }
            else {
              parent = parent._parent
              fix("right")
            }
          }
        }
      }
      def left_or_right(p : AVLNode[(K,V)],child : AVLNode[(K,V)]) : String = {
        if(ord.equiv(p._left._value._1,child._value._1))
          return "left"
        else return "right"
      }
      def connect(grandChild: AVLNode[(K, V)], L_O_R : String): Unit = {
        if(L_O_R == "left"){
          current._parent._left = grandChild
        }
        else {
          current._parent._right = grandChild
        }
      }
      def replace(child : AVLNode[(K,V)], removed : AVLNode[(K,V)]) : Unit = {
        if(!ord.equiv(child._parent._value._1,removed._value._1))
          child._right = removed._right
        child._left = removed._left
        child._rightH = removed._rightH
        child._leftH = removed._leftH
        child._parent = removed._parent
        if(child._left != null){
          child._left._parent = child
        }
        if(child._right != null) {
          current._right._parent = child
        }

        if(removed == _avlRoot)
          _avlRoot = child
        else {
          search = child
        }
      }
      def smallest(root : AVLNode[(K,V)],once : Int = 1): AVLNode[(K,V)] ={
       if(once == 0){
          if (root._right == null) {
            parent = root._parent
            return root._right
          } else smallest(root._right,0)
        }
        if(root._right._left == null) {
          return  root._right
        } else{
          smallest(root._right,0)
        }
      }
    }
    found
  }

  def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    val _parentStack = {
      val stack = new Stack[AVLNode[(K, V)]]
      var currentNode = _avlRoot
      while (currentNode != null) {
        stack.push(currentNode)
        currentNode = currentNode._left
      }
      stack
    }

    override def hasNext: Boolean = _parentStack.nonEmpty

    override def next(): (K, V) = {
      val originalTop = _parentStack.top
      if (originalTop._right != null) {
        var currentNode = originalTop._right
        while (currentNode != null) {
          _parentStack.push(currentNode)
          currentNode = currentNode._left
        }
      }
      else {
        var recentTop = _parentStack.pop
        while (_parentStack.nonEmpty && recentTop != _parentStack.top._left) {
          recentTop = _parentStack.pop
        }
      }
      originalTop._value
    }
  }
  def state(node : AVLNode[(Int,Int)], state : Int = -1) : Boolean = {
    var expect : (Boolean,Boolean) = (false,false)
    if(state == 0)
      expect = (true,false)
    else if(state == 1)
      expect = (false,true)
    val actual = (node._leftH,node._rightH)
    println(actual + " " + expect)
    return (actual._1 == expect._1 && actual._2 == expect._2)
  }
  def height(root: AVLNode[(Int,Int)]): Int = {
    if (root == null) -1
    else 1 + (height(root._left) max height(root._right))
  }
  def testAVL(root:  AVLNode[(Int,Int)]): Unit = {
    if (root == null) {}
    else {
      // visit left.
      testAVL(root._left)
      // visit root.
      val balance = height(root._right) - height(root._left)
      assert(-1 <= balance && balance <= 1)
      if (balance == -1) assert(root._leftH && !root._rightH) // left heavy.
      else if (balance == 0) {
        assert(!root._leftH && !root._rightH)
      } // balanced.
      else assert(!root._leftH && root._rightH) // right heavy.
      // visit right.
      testAVL(root._right)
    }
  }
}
