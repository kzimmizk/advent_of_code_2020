import scala.io.Source

/**
  * Created by kzimmerman on 12/5/20.
  */
object five {
  val filename: String = "./src/main/data/5"
  val seats: List[Seat] =  Source.fromFile(filename).getLines().map(Seat).toList
  val rows: List[Row] = seats.groupBy(_.rowNumber).map(kvp => Row(kvp._1, kvp._2)).toList
  val frontRowNumber: Long = rows.map(_.row).min
  val backRowNumber: Long = rows.map(_.row).max

  def id(row: Long, column: Long): Long = row * 8 + column

  implicit val seatOrdering = new Ordering[Seat] {
    override def compare(x: Seat, y: Seat): Int = {
      x.seatId.compareTo(y.seatId)
    }
  }

  case class Seat(in: String) {
    val rowNumber: Long = Integer.parseInt(in.substring(0, 7).replace('F', '0').replace('B', '1'), 2)
    val columnNumber: Long = Integer.parseInt(in.substring(7, 10).replace('R', '1').replace('L', '0'), 2)
    val seatId: Long = id(rowNumber, columnNumber)
  }

  case class Row(row: Long, seats: Iterable[Seat]) {
    val rowNumber: Long = seats.head.rowNumber
    val emptySeatColumn: Option[Long] =
      Set(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L).diff(seats.map(_.columnNumber).toSet).headOption
    val emptySeatId: Long = id(rowNumber, emptySeatColumn.getOrElse(-1))
  }

  def main(args: Array[String]): Unit = {
    val firstAnswer = seats.max.seatId
    val secondAnswer = rows
      .filter(row => row.row != frontRowNumber && row.row != backRowNumber)
      .filter(row => row.emptySeatColumn.isDefined).head.emptySeatId
    println(s"First Answer: $firstAnswer")
    println(s"Second Answer: $secondAnswer")
  }
}
