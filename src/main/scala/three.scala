import scala.io.Source

/**
  * Created by kzimmerman on 12/3/20.
  */
object three {
  val filename: String = "./src/main/data/3"
  val data: List[Elevation] =  Source.fromFile(filename).getLines().map(Elevation).toList

  case class Elevation(in: String) {
    val mapLength: Int = in.length
    val terrainAsInt: Int = Integer.parseInt(in.replace('.','0').replace('#', '1'), 2)
    def isHit(position: Int): Boolean = ((1 << ((mapLength-1)-(position%mapLength))) & terrainAsInt) > 0
  }

  case class Result(position: Int, hits: Int, rowCount: Int) {
    def skipped: () => Result = () => Result(this.position, this.hits, this.rowCount + 1)
    def incremented: (Int, Int) => Result = (position_delta: Int, hits_delta: Int) =>
      Result(this.position + position_delta, this.hits + hits_delta, this.rowCount + 1)
  }

  def run(x_angle: Int, y_angle: Int): Int = data.foldLeft(Result(0, 0, 0))(
      (r: Result, e: Elevation) => {
        if (r.rowCount % y_angle != 0) r.skipped()
        else r.incremented(x_angle, if (e.isHit(r.position)) 1 else 0)
      }
    ).hits

  def main(args: Array[String]): Unit = {
    val firstAnswer = run(3, 1)
    val secondAnswer: Long = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)).map(x => run(x._1, x._2).toLong).product
    println(s"First Answer: $firstAnswer")
    println(s"Second Answer: $secondAnswer")
  }
}
