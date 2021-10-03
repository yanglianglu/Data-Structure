/**
 * cse250.pa3.TrieTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:
 * Person#:
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa3

import org.scalatest._


class TrieTests extends FlatSpec with BeforeAndAfter {

  behavior of "Trie"
  it should "store the words from the assignment" in {
    val testTrie = new Trie
    val words = List("a", "i", "an", "in", "to", "and", "inn", "tea", "ted", "ten","cating","c","cu")
    val counts = List(12, 11, 22, 5, 7, 17, 9, 3, 4, 12,1,1,1)
    for ((word, count) <- words zip counts; _ <- 0 until count) {
      testTrie.insert(word)
    }

    for ((word, count) <- words zip counts; _ <- 0 until count) {
      assert(testTrie.count(word) == count)
    }
    val mostCommon = Seq("i","a","ten","and","an")
    val mostCommon_1 = Seq("c","cating","cu","tea","ted","in","to","inn","i","a","ten","and","an")
    assert(testTrie.mostCommon(1) == Seq("an"))
    assert(testTrie.mostCommon(5) == mostCommon)
    assert(testTrie.mostCommon(100) == mostCommon_1)
    assert(testTrie.mostCommonWithPrefix("t",5) == Seq("tea","ted","to","ten"))
    assert(testTrie.mostCommonWithPrefix("an",10) == Seq("and","an"))
    assert(testTrie.mostCommonWithPrefix("cse",100) == Seq())
    assert(testTrie.mostCommonWithPrefix("c",100) == Seq("c","cating","cu"))



  }

  behavior of "insert space"
  it should "insert vertex and edge properly" in {
    val trie = new Trie
    trie.insert("")
    val edge = trie._storageGraph.edges
    val vertex = trie._storageGraph.vertices
    assert(edge.length == 0)
    assert(vertex.next().elem == 0)
    assert(vertex.length == 1)
    trie.insert("")
    trie.insert("")
    trie.insert("")

    val first_edge = trie._storageGraph.edges
    val first_vertex = trie._storageGraph.vertices
    assert(first_edge.length == 0)
    assert(first_vertex.length == 1)

  }

  behavior of "insert single char"
  it should "insert vertex and edge properly" in {
    val trie = new Trie
    for(i <- 'a' to 'z')
      trie.insert(i.toString)
    assert(trie._storageGraph.vertices.length == 27)
    assert(trie._storageGraph.edges.length == 26)
    val first_deep = trie._storageGraph.edges
    val first_deep_vertex = trie._storageGraph.vertices
    first_deep_vertex.next()
    for(i <- 'a' to 'z'){
      val edge = first_deep.next()
      val vertex = first_deep_vertex.next()
      assert(edge.elem == i)
      assert(edge.opposite(vertex) == trie._trieRoot)
      assert(vertex.elem == 1)
    }
  }
  behavior of "insert word"
  it should "insert vertex and edge properly" in {
    val trie = new Trie
    trie.insert("hello")
    val edges = trie._storageGraph.edges
    val vertex = trie._storageGraph.vertices
    var root = vertex.next()
    for(i <- "hello"){
      val edge = edges.next()
      assert(edge.elem == i)
      if(i != 'o')
      assert(edge.opposite(root).elem == 0)
      else assert(edge.opposite(root).elem == 1)
      root = vertex.next()
    }
    }
  behavior of "insert multiple word"
  it should "insert vertex and edge properly" in {
    val trie = new Trie
    trie.insert("hello")
    trie.insert("hello")
    trie.insert("hell")
    trie.insert("helle")
    trie.insert("hellee")
    val vertices = trie._storageGraph.vertices
    val edges = trie._storageGraph.edges
    assert(vertices.length == 8)
    assert(edges.length == 7)
    var vertex = vertices.next()
    val e = List('h','e','l','l','o','e','e')
    val v = List(0,0,0,1,2,1,1)
    var i = 0
    while(edges.hasNext){
      val edge = edges.next()
      val ver = vertices.next()
      assert(edge.opposite(vertex).elem == ver)
      assert(edge.elem == e(i))
      assert(ver.elem == v(i))
      i += 1

    }
  }


  behavior of "iterator_1"
  it should "return a functional iterator" in {
    val trie = new Trie
    trie.insert("33")
    val one = trie.iterator
    assert(one.length == 1)
    assert(one.next() == "33")
    val increase = List("a","aa","aa","33","aaaa","aaaaaa","aaaaaaaa","aaaaaaaaaa","aaaaaaaaaaaa","aaaaaaaaaaaaaa","aaaaaaaaaaaaaaaaaa")
    for(i <- increase)
      trie.insert(i)
    val two = trie.iterator
    assert(two.length == increase.length)
    for(i <- increase)
      assert(i == two.next())



  }

  behavior of "iterator_2"
  it should "return a functional iterator" in {
    val trie = new Trie
    for(i <- 0 until 10)
      trie.insert(i.toString)
    val num = trie.iterator
    assert(num.length == 10)
    for(i <- 0 until 10){
      val n = num.next()
      assert(n.toInt == i)
    }



  }
  behavior of "allWordsOfLength"
  it should "be right!" in {
    val trie = new Trie
    assert(trie.allWordsOfLength(5) == Seq())
    assert(trie.allWordsOfLength(0) == Seq())
    trie.insert("c")
    assert(trie.allWordsOfLength(1) == Seq("c"))
    trie.insert("a")
    assert(trie.allWordsOfLength(1) == Seq("a","c"))
    assert(trie.allWordsOfLength(2) == Seq())

  }

  behavior of "mostCommon"
  it should "be right!" in {
    val trie = new Trie


  }
}

