import scala.io.Source

case class Passport(tokens: Set[Token]) {
  def hasRequiredFields: Boolean = {
    val requiredFields = Set("ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt")
    requiredFields.forall(field => tokens.exists(tok => field.equals(tok.key)))
  }

  def isValid: Boolean = {
      tokens.filter(_.key == "hgt").forall(hgt => isValidHgt(hgt.value))
  }

  def isValidHgt(hgtValue: String): Boolean = {
    val regex = "^(\\d+)(\\w+)$".r
    regex.findFirstMatchIn(hgtValue)
      .map()
    val birthYear = tokens.filter(_.key == "byr").map(_.value.toInt).head
    val issueYear = tokens.filter(_.key == "iyr").map(_.value.toInt).head
    val expiryYear = tokens.filter(_.key == "eyr").map(_.value.toInt).head

    birthYear <= 2002 && issueYear <= 2020 && expiryYear >= 2020 &&
      issueYear >= birthYear
  }
}
case class Token(key: String, value: String)

object Day4 extends App {

  val regex = "(\\w{3}):([^\\s]+)".r

  val passportStrings = Source.fromResource("day4").mkString.split("\n\n")

  def tokens(passport: String): Set[Token] = {
    regex.findAllMatchIn(passport)
      .map(m => Token(m.group(1), m.group(2)))
      .toSet
  }

  val passports = passportStrings.map(p => Passport(tokens(p)))

  val validPassports = passports.filter(_.hasRequiredFields)

  println(validPassports.length)

  val validPassports2 = passports.filter(_.hasRequiredFields).filter(_.isValid)

  println(validPassports2.length)
}
