import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by kzimmerman on 12/7/20.
  */
object seven {
  type Bag = String
  val rules: List[Rule] = Source.fromFile("./src/main/data/7").mkString.split("\n").map(Rule.Parse).toList
  val inverted: Map[Bag, List[Bag]] = rules
    .flatMap(r => r.contains.map((x: (Bag, Int)) => (x._1, r.containing)))
    .groupBy(_._1)
    .map { case (k,v) => (k,v.map(_._2))}
  val walkable: Map[Bag, Map[Bag, Int]] = rules
    .map(r => r.containing -> r.contains)
    .toMap

  case class Rule(containing: Bag, contains: Map[Bag, Int])

  object Rule {
    //clear lime bags contain 1 plaid green bag, 3 pale gold bags, 2 bright gray bags.
    val line: Regex =  raw"^(.*) bags contain (.*)\.$$".r
    val containsPattern: Regex = raw"[\ ]*(\d) (.*) bag[s]*".r
    def Parse(in: String): Rule = {
      in match {
        case line(containing, containString) => {
          Rule(
            containing,
            containString.split(',').flatMap {
              case containsPattern(qty, "no other") => None
              case containsPattern(qty, name) => Some(name -> qty.toInt)
              case _ => None
            }.toMap)
        }
      }
    }
  }

  def traverseA(from: Bag): Set[Bag] = {
    inverted.get(from) match {
      case Some(y: List[Bag]) => y.foldLeft(y.toSet)((col: Set[Bag], bag: Bag) => col ++ traverseA(bag))
      case None => Set[Bag]()
    }
  }

  def traverseB(from: Bag): Int = {
    walkable.get(from) match {
      case Some(y: Map[Bag, Int]) => y.foldLeft(1)((sum: Int, bagCount: (Bag, Int)) => traverseB(bagCount._1) * bagCount._2 + sum)
      case None => 0
    }
  }

  def main(args: Array[String]): Unit = {
    val firstAnswer: Int = traverseA("shiny gold").size
    val secondAnswer: Int = traverseB("shiny gold") - 1
    println(s"First Answer: $firstAnswer")
    println(s"Second Answer: $secondAnswer")
  }

}
