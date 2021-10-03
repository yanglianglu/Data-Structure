/**
 * cse250.examples.graph.AdjacencyListGraph.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.examples.graph

class AdjacencyListGraph[VType, EType] extends cse250.examples.types.mutable.GraphADT[VType, EType] {

  private class ListPosition[T](var _value: T, var _prev: ListPosition[T], var _next: ListPosition[T])

  private var _vertexListHead: ListPosition[Vertex] = null
  private var _vertexListTail: ListPosition[Vertex] = null
  private var _edgeListHead: ListPosition[Edge] = null
  private var _edgeListTail: ListPosition[Edge] = null

  class AdjListVertex(override var _elem: VType) extends Vertex {
    private[AdjacencyListGraph] val _position = new ListPosition[Vertex](this, null, null)
    private[AdjacencyListGraph] var _adjListHead: ListPosition[Edge] = null
    private[AdjacencyListGraph] var _adjListTail: ListPosition[Edge] = null
    def elem_=(newElem: VType): Unit = _elem = newElem
  }

  case class AdjListEdge(override val _u: Vertex, override val _v: Vertex, override val _elem: EType) extends Edge {
    private[AdjacencyListGraph] val _position = new ListPosition[Edge](this, null, null)
    private[AdjacencyListGraph] val _adjListPositionU = new ListPosition[Edge](this, null, null)
    private[AdjacencyListGraph] val _adjListPositionV = new ListPosition[Edge](this, null, null)
  }

  override def insertVertex(label: VType): Vertex = {
    val v = new AdjListVertex(label)
    v._position._prev = _vertexListTail
    if (_vertexListHead == null) _vertexListHead = v._position
    else _vertexListTail._next = v._position
    _vertexListTail = v._position
    v
  }

  override def insertEdge(u: Vertex, v: Vertex, label: EType): Edge = {
    val e = new AdjListEdge(u, v, label)
    e._position._prev = _edgeListTail
    if (_edgeListHead == null) _edgeListHead = e._position
    else _edgeListTail._next = e._position

    val adjU = u.asInstanceOf[AdjListVertex]
    e._adjListPositionU._prev = adjU._adjListTail
    if (adjU._adjListHead == null) adjU._adjListHead = e._adjListPositionU
    else adjU._adjListTail._next = e._adjListPositionU
    adjU._adjListTail = e._adjListPositionU

    val adjV = v.asInstanceOf[AdjListVertex]
    e._adjListPositionV._prev = adjV._adjListTail
    if (adjV._adjListHead == null) adjV._adjListHead = e._adjListPositionV
    else adjV._adjListTail._next = e._adjListPositionV
    adjV._adjListTail = e._adjListPositionV

    _edgeListTail = e._position
    e
  }

  override def removeVertex(v: Vertex): Unit = {
    // Remove all edges v appears in from the graph.
    for (e <- this.incidentEdges(v)) this.removeEdge(e)
    // Remove v from vertex sequence.
    val pos = v.asInstanceOf[AdjListVertex]._position
    if (_vertexListHead == pos) {
      _vertexListHead = _vertexListHead._next
      if (_vertexListHead == null) _vertexListTail = null
      else _vertexListHead._prev = null
    }
    else if (_vertexListTail == pos) {
      _vertexListTail = _vertexListTail._prev
      _vertexListTail._next = null
    }
    else {
      // Position interior to list. Relink around vertex.
      pos._prev._next = pos._next
      pos._next._prev = pos._prev
    }
  }

  override def removeEdge(e: Edge): Unit = {
    // Remove e from edge sequence.
    val pos = e.asInstanceOf[AdjListEdge]._position
    if (_edgeListHead == pos) {
      _edgeListHead = _edgeListHead._next
      if (_edgeListHead == null) _edgeListTail = null
      else _edgeListHead._prev = null
    }
    else if (_edgeListTail == pos) {
      _edgeListTail = _edgeListTail._prev
      _edgeListTail._next = null
    }
    else {
      // Position interior to list. Relink around vertex.
      pos._prev._next = pos._next
      pos._next._prev = pos._prev
    }
    // Remove edge from u adjacency list.
    val u = e.asInstanceOf[AdjListEdge]._u.asInstanceOf[AdjListVertex]
    val uPos = e.asInstanceOf[AdjListEdge]._adjListPositionU
    if (u._adjListHead == uPos) {
      u._adjListHead = u._adjListHead._next
      if (u._adjListHead == null) u._adjListTail = null
      else u._adjListHead._prev = null
    }
    else if (u._adjListTail == uPos) {
      u._adjListTail = u._adjListTail._prev
      u._adjListTail._next = null
    }
    else {
      // Position interior to list. Relink around vertex.
      uPos._prev._next = uPos._next
      uPos._next._prev = uPos._prev
    }
    // Remove edge from v adjacency list.
    val v = e.asInstanceOf[AdjListEdge]._v.asInstanceOf[AdjListVertex]
    val vPos = e.asInstanceOf[AdjListEdge]._adjListPositionV
    if (v._adjListHead == vPos) {
      v._adjListHead = v._adjListHead._next
      if (v._adjListHead == null) v._adjListTail = null
      else v._adjListHead._prev = null
    }
    else if (v._adjListTail == vPos) {
      v._adjListTail = v._adjListTail._prev
      v._adjListTail._next = null
    }
    else {
      // Position interior to list. Relink arovnd vertex.
      vPos._prev._next = vPos._next
      vPos._next._prev = vPos._prev
    }
  }

  override def vertices: Iterator[Vertex] = new Iterator[Vertex] {
    private var _currentVertexPosition = _vertexListHead

    override def hasNext: Boolean = _currentVertexPosition != null

    override def next(): Vertex = {
      val retval = _currentVertexPosition._value
      _currentVertexPosition = _currentVertexPosition._next
      retval
    }
  }

  override def edges: Iterator[Edge] = new Iterator[Edge] {
    private var _currentEdgePosition = _edgeListHead

    override def hasNext: Boolean = _currentEdgePosition != null

    override def next(): Edge = {
      val retval = _currentEdgePosition._value
      _currentEdgePosition = _currentEdgePosition._next
      retval
    }
  }

  override def incidentEdges(v: Vertex): Iterator[Edge] = new Iterator[Edge] {
    private var _currentEdgePosition = v.asInstanceOf[AdjListVertex]._adjListHead

    override def hasNext: Boolean = _currentEdgePosition != null

    override def next(): Edge = {
      val retval = _currentEdgePosition._value
      _currentEdgePosition = _currentEdgePosition._next
      retval
    }
  }

  override protected def isAdjacentTo(u: Vertex, v: Vertex): Boolean = {
    val uPosition = incidentEdges(u)
    val vPosition = incidentEdges(v)
    while (uPosition.hasNext && vPosition.hasNext) {
      if (uPosition.next.opposite(u) == v || vPosition.next.opposite(v) == u)
        return true
    }
    false
  }
}