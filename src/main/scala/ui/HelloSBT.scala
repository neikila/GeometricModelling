package ui

import javafx.event.EventHandler
import javafx.scene.input.MouseEvent

import constraint.Axis
import form.Point.PointId
import form.{Line, Point}
import solver.Model

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.{implicitConversions, postfixOps}
import scalafx.application.JFXApp
import scalafx.geometry.Point2D
import scalafx.scene.layout.Pane
import scalafx.scene.{Node, Scene}
import scalafx.scene.paint.Color._
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import scalafx.scene.shape.{Line => LineFX}

object HelloSBT extends JFXApp {

  def app = this
  var model = createModel()
  var currentId = System.nanoTime()

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
  val pointRadius = 5

  val pane = new Pane
  pane.prefWidth = modelSize.x
  pane.prefHeight = modelSize.y

  var pointsMap = drawPoints.toMap
  var linesMap = drawLines.toMap
  drawField foreach (pane.children.add(_))
  (linesMap.values.toStream #::: pointsMap.values.toStream #::: Stream.empty[Node]) foreach {
    pane.children.add(_)
  }

  stage = new JFXApp.PrimaryStage {
    title.value = "Hello Stage"
    width = stageSize.x
    height = stageSize.y
    scene = new Scene {
      fill = LightGray
      content = pane
    }
  }

  def draw(point: Point): (PointId, Circle) = {
    val center = zeroPoint +- point
    val circle: Circle = new Circle {
      radius = pointRadius
      fill = pointColor
    }
    update(point, circle)
    point.id -> circle
  }

  def draw(line: Line): (Line, LineFX) = {
    implicit val points = model.pointsAr
    val p1 = zeroPoint +- line.fromP
    val p2 = zeroPoint +- line.toP
    val lineFx = new LineFX { stroke = lineColor }
    update(line, lineFx)
    line -> lineFx
  }

  class PointHandlers(private val point: Point, val circle: Circle) {
    var modelPoint: Point = point

    def pressed = new EventHandler[MouseEvent] {
      override def handle(event: MouseEvent): Unit = {
        val (_, newP) = draw(point)
        pointsMap = pointsMap + (point.id -> newP)
        pane.children.add(newP)
        circle.fill = selectedPointColor
      }
    }

    def dragProcess = new EventHandler[MouseEvent] {
      override def handle(event: MouseEvent): Unit = {
        val newCenter = new Point2D(event.getSceneX, event.getSceneY)
        circle.centerX = newCenter.x
        circle.centerY = newCenter.y

        modelPoint = (newCenter invertedDif zeroPoint).copy(id = point.id)
        println(modelPoint)
        update()
      }
    }

    def dragOver = new EventHandler[MouseEvent] {
      override def handle(event: MouseEvent): Unit = {
        pane.children.remove(circle)
      }
    }

    def update(): Unit =  {
      val recalculation = new Recalculation
      recalculation {
        model = model.updatePoint(modelPoint).recalculate
      } onSuccess {
        case (_) => app.synchronized { if (currentId < recalculation.t) updateModel() }
      }
    }
  }

  def drawPoints = model.points.map(draw)
  def drawLines = model.lines.map(draw)

  def drawField: List[Node] = {
    val horizontals = (0.0 to stageSize.y by lineStep.y).map { y =>
      val color = if (y == zeroPoint.y) Red else meshColor
      new LineFX {
        startX = 0
        endX = stageSize.x
        startY = y
        endY = y
        stroke = color
      }
    } toList
    val verticals = (0.0 to stageSize.x by lineStep.x).map { x =>
      val color = if (x == zeroPoint.x) Red else meshColor
      new LineFX {
        startX = x
        endX = x
        startY = 0
        endY = stageSize.y
        stroke = color
      }
    } toList

    horizontals ::: verticals
  }

  def update(point: Point, circle: Circle): Unit = {
    val center = zeroPoint +- point
    circle.centerX = center.x
    circle.centerY = center.y

    val handler: PointHandlers = new PointHandlers(point, circle)
    circle.onMousePressed = handler.pressed
    circle.onMouseDragged = handler.dragProcess
    circle.onMouseReleased = handler.dragOver
  }

  def update(line: Line, lineFX: LineFX): Unit = {
    implicit val points = model.pointsAr
    val p1 = zeroPoint +- line.fromP
    val p2 = zeroPoint +- line.toP

    lineFX.startX = p1.x
    lineFX.startY = p1.y
    lineFX.endX = p2.x
    lineFX.endY = p2.y
  }

  def updateModel(): Unit = {
    model.points.foreach { point => update(point, pointsMap(point.id)) }
    model.lines.foreach { line => update(line, linesMap(line)) }
  }

  def createModel(): Model = {
    val model = new Model
    val point = model.newPoint(5, 7)
    val point2 = model.newPoint(5, 120)

    val c1 = model.fixedAxis(point, Axis.X, 5)
    val c2 = model.fixedAxis(point, Axis.Y, 0)

    val line = model.add(new Line(point, point2))
    val c3 = model.fixedLineLength(line, 9)

    model.recalculate
  }

  implicit class Point2dExtension(val point2D: Point2D) {
    def +(withP: Point2D) = new Point2D(point2D.x + withP.x, point2D.y + withP.y)
    def +-(withP: Point2D) = new Point2D(point2D.x + withP.x, point2D.y - withP.y)
    def -(withP: Point2D) = new Point2D(point2D.x - withP.x, point2D.y - withP.y)

    def invertedDif(withP: Point2D) = new Point2D(point2D.x - withP.x, withP.y - point2D.y)
  }

  implicit def pointToPoint2D(point: Point): Point2D = new Point2D(point.x * scale.x, point.y * scale.y)
  implicit def point2dToPoint(point2d: Point2D): Point = new Point(point2d.x / scale.x, point2d.y / scale.y)
}

class Recalculation {
  val t = System.nanoTime()

  def apply[T](body: =>T) = Future(body)
}
