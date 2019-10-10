package edu.colorado.fitzgero.sotestbed.model

package object roadnetwork {

  type Path = List[PathSegment]
  val EmptyPath: Path = List.empty[PathSegment]

}
