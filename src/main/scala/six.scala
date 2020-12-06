import scala.io.Source

/**
  * Created by kzimmerman on 12/6/20.
  */
object six {
  val data: Array[Array[String]] =  Source.fromFile("./src/main/data/6").mkString.split("\n\n").map(_.split('\n'))

  def main(args: Array[String]): Unit = {
    println(s"First Answer: ${data.map(_.map(_.toSet).reduce((a, b) => a.union(b)).size).sum}")
    println(s"Second Answer: ${data.map(_.map(_.toSet).reduce((a, b) => a.intersect(b)).size).sum}")
  }

}
