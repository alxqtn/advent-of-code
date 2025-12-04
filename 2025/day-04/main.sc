import scala.io.Source

enum Location:
  case Paper, Empty

case class ForkliftGrid(grid: List[List[Location]]):
  lazy val accessibleLocations =
    grid.zipWithIndex.collect { (line, i) =>
      line.zipWithIndex.collect {
        case (Location.Paper, j) => isForkliftAccessible(i, j)
        case (Location.Empty, j) => false
      }
    }

  val adjacents = List((0, 1), (0, -1), (1, 0), (-1, 0), (-1, -1), (-1, 1), (1, 1), (1, -1))

  def isForkliftAccessible(i: Int, j: Int) =
    val neighborRolls =
      adjacents.flatMap { (k, l) =>
        val neighbor = grid.lift(i + k).flatMap(_.lift(j + l))
        neighbor match {
          case Some(Location.Paper) => Some(1)
          case Some(Location.Empty) => Some(0)
          case None => None
        }
      }

    neighborRolls.sum < 4

  lazy val collectibleRolls = accessibleLocations.flatten.count(_ == true)

  def collectIteratively: Int =
    if collectibleRolls == 0 then return 0
    val newGrid = grid.zipWithIndex.map { (line, i) =>
      line.zipWithIndex.map { (location, j) =>
        accessibleLocations(i)(j) match {
          case true => Location.Empty
          case false => location
        }
      }
    }

    return collectibleRolls + ForkliftGrid(newGrid).collectIteratively

object PrintingDepartment:
  def one =
    val grid = ForkliftGrid(common("2025/day-04/input.txt"))
    grid.collectibleRolls


  def two =
    val grid = ForkliftGrid(common("2025/day-04/input.txt"))
    grid.collectIteratively

  def common = (fileName: String) =>
    Source
      .fromFile(fileName)
      .getLines
      .map { line =>
        line.split("").toList.collect {
          case "@" => Location.Paper
          case "." => Location.Empty
        }
      }.toList

println(PrintingDepartment.one)
println("=================")
println(PrintingDepartment.two)
