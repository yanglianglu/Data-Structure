/**
 * cse250.pa3.Trie.scala
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
package cse250.pa3

import cse250.examples.graph.AdjacencyListGraph



class Trie {
  /** The graph to store the trie. */
  val _storageGraph = new AdjacencyListGraph[Int,Char]
  /** The root of the trie. */
  val _trieRoot = _storageGraph.insertVertex(0)

  /** Inserts the given word into the trie graph. */
  def insert(word: String): Unit = {
    var current_root = _trieRoot
    for(i: Int <- 0 to word.length-1){
      val edge = _storageGraph.incidentEdges(current_root) //traversal by incident step
      if(current_root != _trieRoot) //first edge always be the parent other than root
        edge.next()
      var exist : Boolean = false
      while(edge.hasNext){
        val elem = edge.next()
        if(elem.elem == word(i)) {
          exist = true//exist then assign root to next
          current_root = elem.opposite(current_root)
        }

      }

      if(exist == false){//not exist create a new child and connect current_root to child
        val vertice = _storageGraph.insertVertex(0)
        _storageGraph.insertEdge(current_root,vertice,word(i))
        current_root = vertice
      }

      if(i == word.length-1)
        current_root._elem += 1

    }

  }

  /** Returns the number of times the given word was inserted. */
  def count(word: String): Int = {
    var current_root = _trieRoot
    for(i: Int <- 0 to word.length-1){
      val edge = _storageGraph.incidentEdges(current_root) //traversal by incident step
      if(current_root != _trieRoot) //first edge always be the parent other than root
        edge.next()
      var exist : Boolean = false
      while(edge.hasNext){
        val elem = edge.next()
        if(elem.elem == word(i)) {
          exist = true//exist then assign root to next
          current_root = elem.opposite(current_root)
        }
      }

      if(exist == false){//not exist create a new child and connect current_root to child
        return 0
      }

      if(i == word.length-1){
        return current_root.elem
      }


    }
    return 0
  }

  /** Returns the number of words stored within. */
  def length: Int = {
    var len = 0
    val vertice = _storageGraph.vertices
    while(vertice.hasNext){
      len += vertice.next().elem
    }
    return len
  }

  /** Returns an Iterator that can be used only once. */
  def iterator: Iterator[String] = new Iterator[String]{
    var deep : cse250.examples.list.LectureImmutableLinkedList[Trie.this._storageGraph.Vertex] = cse250.examples.list.EmptyList
    var unSort : cse250.examples.list.LectureImmutableLinkedList[String] = cse250.examples.list.EmptyList
    val vertices = _storageGraph.vertices

    while(vertices.hasNext){
      var vertice = vertices.next()
      val edge = _storageGraph.incidentEdges(vertice)
      if(edge.length <= 1 && vertice != _trieRoot)//deepest vertices only have one connection
        deep = deep.inserted(0,vertice)
    }


    for(i <- deep){
      var sub : cse250.examples.list.LectureImmutableLinkedList[String] = cse250.examples.list.EmptyList
      var edge = _storageGraph.incidentEdges(i).next()
      var vertice = i


      while(vertice != _trieRoot){
        if(sub.isEmpty){
            sub = sub.inserted(0,edge.elem.toString)//insert element
        }
        else{

          for(i <- 0 until sub.length) {
            sub = sub.updated(i,sub(i) + edge.elem)//each time vertice going up, add edge.elem to the list
          }
          if(vertice.elem > 0) {
            sub = sub.inserted(0,edge.elem.toString)//if vertice.elem > 0, insert edge.elem
          }
        }
        vertice = edge.opposite(vertice)
        edge = _storageGraph.incidentEdges(vertice).next()
      }


      for(i <- 0 until sub.length){
        val str = sub(i).reverse

        if(!unSort.contains(str))
          unSort = unSort.inserted(0,str)
      } //insert to accumulate list
    }

    val words  = unSort.sortWith(_.length < _.length)
    var len = 0
    override def hasNext: Boolean = len < words.length

    override def next(): String = {
      val reval = words(len)
      len += 1
      reval
    }
  }

  /** Returns a sequence of all words of a given length ordered alphabetically. */
  def allWordsOfLength(length: Int): Seq[String] = {
    val words = iterator
    var seq : Seq[String] = Seq()
    while(words.hasNext){
      val word = words.next()
      if(word.length == length)
        seq = seq.appended(word)
    }
    seq.sortWith(_ < _)
  }

  /** Returns a sequence containing the k most inserted words.*/
  def mostCommon(k: Int): Seq[String] = {
    val words = iterator
    var seq : Seq[String] = Seq()
    while(words.hasNext){
      val word = words.next()
        seq = seq.appended(word)
    }
    def sort(str1 : String,str2 : String): Boolean ={
      if(count(str1) < count(str2))
        return true
      else {
        if(count(str1) == count(str2) && str1 < str2)
          return true
        else return false
      }
    }
    seq = seq.sortWith(sort(_,_))

    if(seq.length > k)
      return seq.drop(seq.length  - k)
    else return seq
  }

  /** Returns a sequence containing the k most inserted words that start with the given prefix. */
  def mostCommonWithPrefix(prefix: String, k: Int): Seq[String] = {
    val words = iterator
    var seq : Seq[String] = Seq()
    while(words.hasNext){
      val word = words.next()
      if(word.startsWith(prefix))
      seq = seq.appended(word)
    }
    def sort(str1 : String,str2 : String): Boolean ={
      if(count(str1) < count(str2))
        return true
      else {
        if(count(str1) == count(str2) && str1 < str2)
          return true
        else return false
      }
    }
    seq = seq.sortWith(sort(_,_))

    if(seq.length > k)
      return seq.drop(seq.length  - k)
    else return seq
  }
}
