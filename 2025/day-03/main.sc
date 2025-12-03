import scala.io.Source

case class BatteryBank(batteries: List[Int]):
  def highestJoltage(digits: Int) =
    def process(batteries: List[Int], n: Int): Long =
      if n < 0 then return 0
      val max = batteries.dropRight(n).max
      val maxIndex = batteries.indexOf(max)
      max * Math.pow(10, n).toLong + process(batteries.drop(maxIndex + 1), n - 1)

    process(batteries, digits - 1)


object Lobby:
  def one =
    common("2025/day-03/input.txt", 2)

  def two =
    common("2025/day-03/example.txt", 12)

  def common = (fileName: String, digits: Int) =>
    Source
      .fromFile(fileName)
      .getLines
      .map(line => BatteryBank(line.split("").toList.map(_.toInt)))
      .map(_.highestJoltage(digits)).sum

println(Lobby.one)
println("=================")
println(Lobby.two)
