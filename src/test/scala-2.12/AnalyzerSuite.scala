import constraint.{Axis, Constraint, ConstraintConstructor}
import form.{Line, Point, PointConstructor}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import solver.{Accuracy, Solver}

import scala.language.postfixOps

@RunWith(classOf[JUnitRunner])
class AnalyzerSuite extends FunSuite with Accuracy {

  import Main.solve
  import Solver.pointsToIndices

  test("Fixed line length constraint") {

    // Arrange
    val constraintConstructor = new ConstraintConstructor
    val pointConstructor = new PointConstructor

    val point = pointConstructor.newPoint(0, 0)
    val point2 = pointConstructor.newPoint(9, 20)

    val c1 = constraintConstructor.fixedAxis(point, Axis.X, 0)
    val c2 = constraintConstructor.fixedAxis(point, Axis.Y, 0)

    val line = new Line(point, point2)
    val c3 = constraintConstructor.fixedLineLength(line, 9)

    val constraints: List[Constraint] = c1 :: c2 :: c3 :: Nil
    val forms: List[Point] = point :: point2 :: Nil

    // Action
    val result = solve(forms, constraints).toList

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
    val constraintConstructor = new ConstraintConstructor
    val pointConstructor = new PointConstructor

    val point = pointConstructor.newPoint(5, 3)
    val point2 = pointConstructor.newPoint(5, 23)
    val c1 = constraintConstructor.fixedAxis(point, Axis.X, 5)
    val c2 = constraintConstructor.fixedAxis(point, Axis.Y, 10)

    val line = Line(0, 1)
    val c3 = constraintConstructor.fixedLineLength(line, 5)

    val constraints: List[Constraint] = c1 :: c2 :: c3 :: Nil
    val forms: List[Point] = point :: point2 :: Nil

    // Action
    val result = solve(forms, constraints).toList

    val p1 = result.head
    val p2 = result(1)

    // Assert
    val squareLen = new Line(p1, p2).squareLength(Solver.createSource(p1 :: p2 :: Nil))
    assert(squareLen - c3.squareLength < epsilon)
  }
}