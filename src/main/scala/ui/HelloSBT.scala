package ui

import constraint.Axis
import form.{Line, Point}
import solver.Model

import scala.language.{implicitConversions, postfixOps}
import scalafx.application.JFXApp
import scalafx.geometry.{Point2D, Point3D}
import scalafx.scene.{Node, Scene}
import scalafx.scene.paint.Color._
import scalafx.scene.paint.{Color, LinearGradient, Stops}
import scalafx.scene.shape.{Circle, Rectangle}
import scalafx.scene.shape.{Line => LineFX}
import scalafx.scene.text.Text

object HelloSBT extends JFXApp {

  var model = createModel()

  val strokeTaskTypeColor = Black
  strokeTaskTypeColor.opacity(0.8)

  val meshColor = Color(100.0 / 256, 100.0 / 256, 100.0 / 256, 0.7)
  val pointColor = Orange
  val selectedPointColor = Red
  val lineColor = Blue

  def modelSize = new Point2D(100.toDouble, 100.toDouble)
  def stageSize = new Point2D(600.toDouble, 600.toDouble)
  def zeroPoint = new Point2D(stageSize.x / 2, stageSize.y / 2)
  def lineStep: Point2D = Point(-1, 10, 10)
  val scale = new Point2D(stageSize.x / modelSize.x, stageSize.y / modelSize.y)

  val pointRadius = 3

  stage = new JFXApp.PrimaryStage {
    title.value = "Hello Stage"
    width = stageSize.x
    height = stageSize.y
    scene = new Scene {
      fill = LightGray
      content = drawField ::: drawLines ::: drawPoints
    }
  }

  def draw(point: Point): Node = {
    val center = zeroPoint +- point
    new Circle {
      centerX = center.x
      centerY = center.y
      radius = pointRadius
      fill = pointColor
    }
  }

  def drawLine(line: Line): Node = {
    implicit val points = model.pointsAr
    val p1 = zeroPoint +- line.fromP
    val p2 = zeroPoint +- line.toP
    new LineFX {
      startX = p1.x
      startY = p1.y
      endX = p2.x
      endY = p2.y
      stroke = lineColor
    }
  }

  def drawPoints = model.points.map(draw)
  def drawLines = model.lines.map(drawLine)

  def drawField: List[Node] = {
    val horizontals = (0.0 to stageSize.y by lineStep.y).map { y =>
      new LineFX {
        startX = 0
        endX = stageSize.x
        startY = y
        endY = y
        stroke = meshColor
      }
    } toList
    val verticals = (0.0 to stageSize.x by lineStep.x).map { x =>
      new LineFX {
        startX = x
        endX = x
        startY = 0
        endY = stageSize.y
        stroke = meshColor
      }
    } toList

    horizontals ::: verticals
  }

  implicit class Point2dExtension(val point2D: Point2D) {
    def + (withP: Point2D) = new Point2D(point2D.x + withP.x, point2D.y + withP.y)
    def +- (withP: Point2D) = new Point2D(point2D.x + withP.x, point2D.y - withP.y)
    def - (withP: Point2D) = new Point2D(point2D.x - withP.x, point2D.y - withP.y)
  }

  implicit def pointToPoint2D(point: Point): Point2D = new Point2D(point.x * scale.x, point.y * scale.y)

  def createModel(): Model = {
    var model = new Model
    val point = model.newPoint(5, 7)
    val point2 = model.newPoint(5, 120)

    val c1 = model.fixedAxis(point, Axis.X, 5)
    val c2 = model.fixedAxis(point, Axis.Y, 0)

    val line = model.add(new Line(point, point2))
    val c3 = model.fixedLineLength(line, 9)

    model = model.recalculate
    model.constraints.foreach(println)
    model.pointsAr.foreach(println)

    model
  }
}
