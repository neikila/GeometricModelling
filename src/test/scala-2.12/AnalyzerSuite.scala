import constraint.{Axis, Constraint, ConstraintConstructor}
import form.{Line, Point, PointConstructor}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import solver.{Accuracy, Model, Solver}

import scala.language.postfixOps

@RunWith(classOf[JUnitRunner])
class AnalyzerSuite extends FunSuite with Accuracy {

  import Solver.pointsToIndices

  test("Fixed line length constraint") {

    // Arrange
    val model = new Model
    val point = model.newPoint(0, 0)
    val point2 = model.newPoint(9, 20)

    val c1 = model.fixedAxis(point, Axis.X, 0)
    val c2 = model.fixedAxis(point, Axis.Y, 0)

    val line = new Line(point, point2)
    val c3 = model.fixedLineLength(line, 9)

    // Action
    val result = model.recalculate.recalculated

    val p1 = result.head
    val p2 = result(1)

    // Assert
    val squareLen = new Line(p1, p2).squareLength(Solver.createSource(p1 :: p2 :: Nil))
    assert(squareLen - c3.squareLength < epsilon)
    assert(math.abs(p1.x - c1.value) < epsilon)
    assert(math.abs(p1.y - c2.value) < epsilon)
  }

  test("Infinite amount of solutions matrix") {

    // Arrange

    val model = new Model
    val point = model.newPoint(5, 3)
    val point2 = model.newPoint(5, 23)
    val c1 = model.fixedAxis(point, Axis.X, 5)
    val c2 = model.fixedAxis(point, Axis.Y, 10)

    val line = Line(0, 1)
    val c3 = model.fixedLineLength(line, 5)

    // Action
    val result = model.recalculate.recalculated

    val p1 = result.head
    val p2 = result(1)

    // Assert
    val squareLen = new Line(p1, p2).squareLength(Solver.createSource(p1 :: p2 :: Nil))
    assert(squareLen - c3.squareLength < epsilon)
  }
}