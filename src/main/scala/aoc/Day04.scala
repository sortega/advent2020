package aoc

object Day04 {
  val polarFiels = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  case class Passport(fields: Map[String, String]) {
    def validate: Boolean = polarFiels.subsetOf(fields.keySet)
    def strictValidate: Boolean = 
      validate &&
        validYear(fields("byr"), 1920 to 2002) &&
        validYear(fields("iyr"), 2010 to 2020) &&
        validYear(fields("eyr"), 2020 to 2030) &&
        validHeight &&
        validHairColor &&
        validEyeColor &&
        validPassportId

    private def validYear(year: String, range: Range): Boolean =
      year.matches("""\d{4}""") && range.contains(year.toInt)

    private def validHeight: Boolean = {
      val Pattern = """(\d+)(cm|in)""".r
      fields("hgt").match {
        case Pattern(value, "cm") => (150 to 193).contains(value.toInt)
        case Pattern(value, "in") => (59 to 76).contains(value.toInt)
        case _ => false
      }
    }

    private def validHairColor: Boolean = fields("hcl").matches("""#[0-9a-f]{6}""")

    private def validEyeColor: Boolean = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(fields("ecl"))

    private def validPassportId: Boolean = fields("pid").matches("""\d{9}""")
  }

  object Passport {
    def parseAll(input: Iterator[String]): List[Passport] =
      if (input.isEmpty) Nil
      else {
        val (firstParagraph, rest) = input.span(_.trim.nn.nonEmpty)
        parse(firstParagraph) :: parseAll(rest.drop(1))
      }

    def parse(lines: Iterator[String]): Passport =
      Passport(lines.flatMap(_.safeSplit(" ")).map(f => parseField(f.nn)).toMap)

    private def parseField(string: String): (String, String) = string.safeSplit(":") match {
      case List(field, value) => (field, value)
    }
  }

  def part1(passports: List[Passport]): Int = passports.count(_.validate)

  def part2(passports: List[Passport]): Int = passports.count(_.strictValidate)

  def main(args: Array[String]): Unit = {
    val input = Passport.parseAll(readInputLines(day = 4))
    println(part1(input))
    println(part2(input))
  }
}
