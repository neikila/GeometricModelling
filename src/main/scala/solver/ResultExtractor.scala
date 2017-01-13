package solver

import form.Point

import Solver.Vector
/**
  * Created by k.neyman on 05.01.2017.
  */
class ResultExtractor(val vector: Vector, pointsAmount: Int) {
  def extract: Iterator[Point] = vector.toStream.take(2 * pointsAmount).sliding(2, 2).map(_.toList).zipWithIndex.map {
    case ((x :: y :: Nil), id) => Point(id, x, y)
    case _ => Point(-1, 0, 0) // should never be used
  }
}
