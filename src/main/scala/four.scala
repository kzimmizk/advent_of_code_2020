import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by kzimmerman on 12/4/20.
  */
object four {

  val KV: Regex = raw"(.*):(.*)".r
  val filename: String = "./src/main/data/4"
  val data: Array[Map[String, String]] =  Source.fromFile(filename)
    .mkString.split("\n\n")
    .map(_.replace('\n', ' ').split(' '))
    .map(document => document.map { case KV(key, value) => key -> value}.toMap )

  val requiredFields: Set[String] = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  object Validator {
    def yearPatternMatcher(str: String): Boolean = raw"^[0-9]{4}$$".r.pattern.matcher(str).matches
    def apply(key: String, value: String): Boolean = {
      (key, value) match {
        case ("byr", byr) => yearPatternMatcher(byr) && byr.toInt >= 1920 && byr.toInt <= 2002
        case ("iyr", iyr) => yearPatternMatcher(iyr) && iyr.toInt >= 2010 && iyr.toInt <= 2020
        case ("eyr", eyr) => yearPatternMatcher(eyr) && eyr.toInt >= 2020 && eyr.toInt <= 2030
        case ("hgt", hgt) => {
          val filter = raw"^(\d*)(in|cm)$$".r
          hgt match {
            case filter(qty, metric) => {
              metric match {
                case "cm" => qty.toInt >= 150 && qty.toInt <= 193
                case "in" => qty.toInt >= 59 && qty.toInt <= 76
                case _ => false
              }
            }
            case _ => false
          }
        }
        case ("hcl", hcl) => raw"^#[0-9,a-f]{6}$$".r.pattern.matcher(hcl).matches
        case ("ecl", ecl) => raw"^(amb|blu|brn|gry|grn|hzl|oth)$$".r.pattern.matcher(ecl).matches
        case ("pid", pid) => raw"^\d{9}$$".r.pattern.matcher(pid).matches()
        case _ => true
      }
    }
  }


  def main(args: Array[String]): Unit = {
    val firstAnswer = data.count(document => requiredFields.subsetOf(document.keySet))
    val secondAnswer = data
      .filter(document => requiredFields.subsetOf(document.keySet))
      .count(document => document.forall(kvp => Validator(kvp._1, kvp._2)))
    println(s"First Answer: $firstAnswer")
    println(s"Second Answer: $secondAnswer")
  }
}
