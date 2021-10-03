import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

object Solution {

  class DoublyLinkedListNode(var data: Int, var next: DoublyLinkedListNode = null, var prev: DoublyLinkedListNode = null) {
  }

  class DoublyLinkedList(var head: DoublyLinkedListNode = null, var tail: DoublyLinkedListNode = null) {
    def insertNode(nodeData: Int) = {
      val node = new DoublyLinkedListNode(nodeData)

      if (this.head == null) {
        this.head = node
      } else {
        this.tail.next = node
        node.prev = this.tail
      }

      this.tail = node
    }
  }

  def printDoublyLinkedList(head: DoublyLinkedListNode, sep: String, printWriter: PrintWriter) = {
    var node = head

    while (node != null) {
      printWriter.print(node.data)

      node = node.next

      if (node != null) {
        printWriter.print(sep)
      }
    }
  }
  // Complete the sortedInsert function below.

  /*
   * For your reference:
   *
   * DoublyLinkedListNode {
   *     data: Int
   *     next: DoublyLinkedListNode
   *     prev: DoublyLinkedListNode
   * }
   *
   */
  def sortedInsert(llist: DoublyLinkedListNode, data: Int): DoublyLinkedListNode = {
    val node = new DoublyLinkedListNode(4)
    var temp = llist
    if(llist == null)
        return node
    else if(llist.next == null){
      if(llist.data > data){
        node.next = temp
        temp = node
        return temp
      }
      else {
        llist.next = node
        return llist
      }
    }
    else{
      while(temp != null){
        if(temp.data > data){
          node.prev = temp.prev
          node.next = temp
          temp.prev.next = node
          temp.prev = node
        }
        else{
          temp = temp.next
        }
      }
    }
    return llist
  }
  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val t = stdin.readLine.trim.toInt

    for (tItr <- 1 to t) {
      val llist = new DoublyLinkedList()

      val llistCount = stdin.readLine.trim.toInt

      for (_ <- 0 until llistCount) {
        val llistItem = stdin.readLine.trim.toInt
        llist.insertNode(llistItem)
      }

      val data = stdin.readLine.trim.toInt

      val llist1 = sortedInsert(llist.head, data)

      printDoublyLinkedList(llist1, " ", printWriter)
      printWriter.println()
    }

    printWriter.close()
  }
}