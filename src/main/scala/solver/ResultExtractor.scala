package solver

import form.Point

/**
  * Created by k.neyman on 05.01.2017.
  */
class ResultExtractor(val ab: AB, pointsAmount: Int) {
  def extract = ab.B.toStream.take(2 * pointsAmount).sliding(2, 2).map(_.toList).map {
    case (x :: y :: Nil) => Point(x, y)
  }
}
