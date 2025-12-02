import scala.io.Source

case class IDRange(start: Long, end: Long):
  val range = (start to end)

  def invalidPairs =
    range
      .map(_.toString())
      .filter(_.length() % 2 == 0)
      .flatMap { str =>
        val (first, second) = str.splitAt(str.length() / 2)
        if first == second then Some(str.toLong) else None
      }
      .toList

  def invalidRepeats =
    range
      .map(_.toString())
      .flatMap { str =>
        val possibleDividers = 1 to Math.floor(str.length() / 2).toInt
        possibleDividers
          .filter(str.length() % _ == 0)
          .flatMap { div =>
            println((str, div))
            val chunks = str.grouped(div).toList
            if chunks.distinct.size == 1 then Some(str.toLong) else None
          }
          .distinct
      }

object GiftShop:
  def one =
    val idRanges = common("2025/day-02/input.txt")
    idRanges.flatMap(_.invalidPairs).sum

  def two =
    val idRanges = common("2025/day-02/input.txt")
    idRanges.flatMap(_.invalidRepeats).sum

  def common = (fileName: String) =>
    Source
      .fromFile(fileName)
      .getLines
      .map(_.split(","))
      .flatten
      .map { rangeString =>
        val bounds = rangeString.split("-").map(_.toLong)
        IDRange(bounds(0), bounds(1))
      }

GiftShop.one
GiftShop.two
