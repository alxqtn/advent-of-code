import scala.io.Source

case class BatteryBank(batteries: List[Int]):
  def highestJoltage =
    val first = batteries.dropRight(1).max
    val firstIndex = batteries.indexOf(first)
    val second = batteries.drop(firstIndex + 1).max
    first * 10 + second

object Lobby:
  def one =
    val banks = common("2025/day-03/input.txt")
    banks.map(_.highestJoltage).sum

  def two =
    common("2025/day-03/example.txt")

  def common = (fileName: String) =>
    Source
      .fromFile(fileName)
      .getLines
      .map(line => BatteryBank(line.split("").toList.map(_.toInt)))
      .toList

Lobby.one
Lobby.two
