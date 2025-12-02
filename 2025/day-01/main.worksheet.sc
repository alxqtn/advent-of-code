import scala.util.matching.Regex
import scala.io.Source

enum Direction:
  case Left, Right

case class Rotation(direction: Direction, steps: Int)

class Dial:
  val dials = (0 to 99)
  var position = 50
  var zeros = 0

  def runRotations(rotations: List[Rotation]) =
    rotations.foldLeft(Vector(position)) { (acc, rotation) =>
      rotate(rotation)
      acc :+ position
    }

  def rotate(rotation: Rotation) =
    rotation match {
      case Rotation(Direction.Left, steps) =>
        for _ <- (1 to steps) do
          previous
          if position == 0 then zeros += 1
      case Rotation(Direction.Right, steps) =>
        for _ <- (1 to steps) do
          next
          if position == 0 then zeros += 1
    }

  def next =
    position = if position == dials.last then dials.head else position + 1

  def previous =
    position = if position == dials.head then dials.last else position - 1

object Password:
  val startingPoint = 50
  val dials = (0 to 99)

  def one =
    val history = Dial().runRotations(common("2025/day-01/input.txt"))
    history.count(_ == 0)

  def two =
    val dial = Dial()
    dial.runRotations(common("2025/day-01/input.txt"))
    dial.zeros

  def common = (fileName: String) =>
    val lines = Source.fromFile(fileName).getLines.toList
    getRotations(lines)

  def getRotations(lines: List[String]) =
    for line <- lines yield {
      val dir = if line.head == 'L' then Direction.Left else Direction.Right
      val steps = line.tail.toInt
      Rotation(dir, steps)
    }

Password.one
Password.two
