import scala.io.Source

/**
  * Created by kzimmerman on 12/2/20.
  */
object two {
  val filename = "./src/main/data/2a"
  val line = raw"(\d+)-(\d+) ([a-z]{1}): ([a-z]*)".r
  val data = Source.fromFile(filename).getLines().map {
    case line(minCount, maxCount, character, password) =>
      Password(minCount.toInt, maxCount.toInt, character.toCharArray.head, password)
  }

  case class Password(minCount: Int, maxCount: Int, character: Char, password: String) {
    val count: Int = password.count(_ == character)
    val valid: Boolean = (minCount <= count) &&  (maxCount >= count)
  }

  def main(args: Array[String]): Unit = {

    val firstAnswer: Int = data.count(_.valid)

    val secondAnswer: Int = 0

    println(s"First Answer: valid password count is $firstAnswer")

    data.foreach(println)
  }
}
