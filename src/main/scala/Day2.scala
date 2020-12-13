import scala.io.Source

case class Password(min: Int, max: Int, char: Char, password: String)

object Day2 extends App {

  val regex = "(\\d+)-(\\d+)\\s+(\\w):\\s+(\\w+)".r.pattern

  def password(line: String): Option[Password] = {
    val matcher = regex.matcher(line)
    if (matcher.matches()) {
      Some(Password(matcher.group(1).toInt, matcher.group(2).toInt, matcher.group(3).head, matcher.group(4)))
    } else {
      None
    }
  }

  val pws: List[Password] = {
    Source.fromResource("day2").getLines().map(password).collect {
      case Some(pw) => pw
    }.toList
  }

  val validPws = pws.filter(pw => {
    val charCount = pw.password.count(_ == pw.char)
    charCount >= pw.min && charCount <= pw.max
  })

  println(validPws.size)

  val valid2Pws = pws.filter(pw => {
    val firstMatches = pw.password.charAt(pw.min - 1) == pw.char
    val secondMatches = pw.password.charAt(pw.max - 1) == pw.char
    (firstMatches || secondMatches) && !(firstMatches && secondMatches)
  })

  println(valid2Pws.size)
}
