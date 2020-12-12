import scala.io.Source

/**
  * Created by kzimmerman on 12/9/20.
  */
object nine {
  val cipher: List[Long] = Source.fromFile("./src/main/data/9").getLines().map(str => str.toLong).toList
  val Min: Long = Source.fromFile("./src/main/data/test").getLines.map(str => str.toLong).min
  val Max: Long = Source.fromFile("./src/main/data/test").getLines.map(str => str.toLong).max

  def combine(in :List[Long]): List[Long] = {
    in match {
      case x :: y :: Nil => List[Long](x + y)
      case x :: rest => rest.map(y => x + y) ++ combine(rest)
    }
  }

  def getKey(in: List[Long]): Long =
    if (combine(in.take(25)).contains(in.drop(25).head)) getKey(in.drop(1)) else in.drop(25).head

  def getIndex(lookup: Long, in: List[Long], acc: Int): Int = {
    if (in.head == lookup) acc else getIndex(lookup, in.tail, acc + 1)
  }

  def getOtherKey(key: Long, in: Array[Long], slide: Int): Long = {
    val sums = in.sliding(slide).map(_.sum).toList
    if (sums.toSet.contains(key)) {
      println(slide)
      getIndex(key, sums, 0) + slide
    } else {
      getOtherKey(key, in, slide+1)
    }
  }

  def main(args: Array[String]): Unit = {
    val key = getKey(cipher)
    println(getKey(cipher))
    println(getOtherKey(key, cipher.toArray, 2))
    println(Min + Max)
    /* 248131121
    17
    496
    31580383 */
  }

}
