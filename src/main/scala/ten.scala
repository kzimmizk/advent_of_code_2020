import scala.io.Source

/**
  * Created by kzimmerman on 12/10/20.
  */
object ten {
  val input: List[Long] = 0 :: Source.fromFile("./src/main/data/10").getLines().map(str => str.toLong).toList.sorted

  def dp(a: (Long, Long), b: (Long, Long), c: (Long, Long), next: Long, rest: List[Long]): Long = {
    val d: (Long, Long) = (next,
        (if (next-a._1 < 4) a._2 else 0) +
        (if (next-b._1 < 4) b._2 else 0) +
        (if (next-c._1 < 4) c._2 else 0))
    rest match {
      case Nil => d._2
      case x :: y => dp(b, c, d, x, y)
    }
  }

  case class agg(ones: Int, twos: Int, threes: Int)
  def main(args: Array[String]): Unit = {
    val skips = input.sliding(2).map(x => x.tail.head - x.head).toList
    val ones = skips.count(i => i == 1)
    val threes = skips.count(i => i == 3) + 1 // built-in
    val answerOne = ones * threes
    val answerTwo = dp((0, 0), (0, 0), (-3, 1), input.head, input.tail)
    println(s"Answer one: $answerOne")
    println(s"Answer two: $answerTwo")
  }

}

