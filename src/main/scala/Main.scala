import constraint._
import form.{Line, Point, PointConstructor}
import solver.Solver.{Source, Vector}
import solver._

import scala.language.{implicitConversions, postfixOps}

object Main extends Support {
  val model = new Model

  def main(args: Array[String]): Unit = {
    val point = model.newPoint(5, 7)
    val point2 = model.newPoint(5, 120)

    model.fixedAxis(point, Axis.X, 5)
    model.fixedAxis(point, Axis.Y, 0)

    val line = model.add(new Line(point, point2))
    model.fixedLineLength(line, 9)

    model.recalculate
  }
}
