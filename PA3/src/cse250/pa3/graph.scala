package cse250.pa3

import cse250.examples.graph.AdjacencyListGraph
import cse250.examples.list.{LectureArrayList, LecturePositionalLinkedList}

object graph {
  def main(args: Array[String]): Unit = {
    val map: Trie = new Trie
    map.insert("hello")
    map.insert("hello")
    map.insert("hell")
    map.insert("helle")
    map.insert("to")
    map.insert("two")
    map.insert("tw")
    print(map.allWordsOfLength(5))
    for(i <- 'c' to 'h')
      map.insert(i.toString)

      map.insert("a")
      map.insert("a")
      map.insert("a")
      map.insert("z")
    print(map.mostCommon(2))

  }
}
