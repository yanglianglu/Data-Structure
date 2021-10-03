/**
 * cse250.examples.types.mutable.GraphADT.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.examples.types.mutable

/** GraphADT is an interface that any Graph implementation should conform to.
 *
 * @tparam VType the type for the Vertex elements (vertex label type).
 * @tparam EType the type for the Edge elements (edge label type).
 */
trait GraphADT[VType, EType] {

  /** This is the Vertex class for the graph. */
  trait Vertex {
    /** The element associated with the Vertex (vertex label).*/
    var _elem: VType
    /** Gets the element associated with the Vertex object. */
    def elem: VType = _elem
    def apply: VType = _elem

    /** Tests whether this Vertex is adjacent to Vertex v.
     *
     * The call is delegated to the Graph.isAdjacentTo since the functionality
     * of the Graph implementation will dictate how this is computed.
     */
    def isAdjacentTo(v: Vertex): Boolean = GraphADT.this.isAdjacentTo(this, v)

    override def toString: String = _elem.toString
  }

  /** This Edge class can be used as a directed or undirected graph, as necessary. */
  trait Edge {
    /** The source node for the edge, if directed. */
    val _u: Vertex
    /** The destination node for the edge, if directed. */
    val _v: Vertex
    /** The element associated with the Edge (edge label). */
    val _elem: EType

    /** Gets the element associated with the Edge object. */
    def elem: EType = _elem
    def apply: EType = _elem

    /** Gets a list of the endpoints of this edge in the order of _u followed by _v.
     *
     * Note that this maintains the ordering of _u and _v so this can be used as
     * a directed or undirected graph.
     */
    def endVertices: List[Vertex] = List(_u, _v)

    /** Gets the vertex opposite to w, assuming w is _u or _v. */
    def opposite(w: Vertex): Vertex = if (_u == w) _v else _u

    override def toString: String = Seq(_u,_v,_elem).mkString("Edge(",",",")")
  }

  /** Inserts a Vertex storing element label. */
  def insertVertex(label: VType): Vertex

  /** Inserts an Edge storing element label. */
  def insertEdge(u: Vertex, v: Vertex, label: EType): Edge

  /** Removes the Vertex v from the graph, including all Edges incident to v. */
  def removeVertex(v: Vertex): Unit

  /** Removes the Edge e from the graph. */
  def removeEdge(e: Edge): Unit

  /** Returns an Iterator to the Graph vertices that can be used only once. */
  def vertices: Iterator[Vertex]

  /** Returns an Iterator to the Graph edges that can be used only once. */
  def edges: Iterator[Edge]

  /** Returns an Iterator to the edges incident to Vertex v that can be used only once. */
  def incidentEdges(v: Vertex): Iterator[Edge]

  /** Returns true iff two vertices are adjacent within the Graph.
   *
   * This method must be defined by the Graph implementation. Note that
   * the visibility is limited to within the scope of this class. To test
   * adjacency, you must use Vertex.isAdjacentTo.
   */
  protected def isAdjacentTo(u: Vertex, v: Vertex): Boolean

  /** Converts Graph to string by listing all vertices followed by all edges. */
  override def toString = edges.mkString(vertices.mkString("Graph" + "(Vertices(", ", ", "),Edges("),", ","))")
}