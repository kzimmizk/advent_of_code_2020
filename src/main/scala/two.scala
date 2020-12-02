import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by kzimmerman on 12/2/20.
  */
object two {
  val filename: String = "./src/main/data/2a"
  val line: Regex = raw"(\d+)-(\d+) ([a-z]{1}): ([a-z]*)".r
  val data: List[Password] = Source.fromFile(filename).getLines().map {
    case line(minCount, maxCount, character, password) =>
      Password(minCount.toInt, maxCount.toInt, character.toCharArray.head, password)
  }.toList

  case class Password(a: Int, b: Int, char: Char, password: String) {
    val count: Int = password.count(_ == char)
    val validRule1: Boolean = (a <= count) &&  (b >= count)
    val validRule2: Boolean = (password.charAt(a-1) == char) ^ (password.charAt(b-1) == char)
  }

  def main(args: Array[String]): Unit = {
    val firstAnswer: Int = data.count(_.validRule1)
    val secondAnswer: Int = data.count(_.validRule2)
    println(s"First Answer: valid password count is $firstAnswer")
    println(s"Second Answer: valid password count is $secondAnswer")
  }
}
