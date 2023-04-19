package u06lab.code

abstract class Parser[T]:
  def parse(t: T): Boolean // is the token accepted?
  def end: Boolean // is it ok to end here
  def parseAll(seq: Seq[T]): Boolean = (seq forall parse) & end // note &, not &&

/* Extension for Scala String*/
extension (string: String)
  def charParser: BasicParser = new BasicParser(string.toSet)
class BasicParser(chars: Set[Char]) extends Parser[Char]:
  override def parse(t: Char): Boolean = chars.contains(t)
  override def end: Boolean = true

trait NonEmpty[T] extends Parser[T]:
  private[this] var empty = true
  abstract override def parse(t: T): Boolean =
    empty = false;
    super.parse(t)
  abstract override def end: Boolean = !empty && super.end

class NonEmptyParser(chars: Set[Char]) extends BasicParser(chars) with NonEmpty[Char]

trait NotTwoConsecutive[T] extends Parser[T]:
  private[this] var previous: T = _

  abstract override def parse(t: T): Boolean =
    if previous != t then
      previous = t
      super.parse(t)
    else
      false
  abstract override def end: Boolean = super.end

trait ShortenThanN[T] extends Parser[T]:
  private[this] var n = 0
  def max: Int
  abstract override def parse(t: T): Boolean =
    if n < max then
      n = n + 1
      super.parse(t)
    else
      false
  abstract override def end: Boolean = super.end
class NotTwoConsecutiveParser(chars: Set[Char]) extends BasicParser(chars) with NotTwoConsecutive[Char]

class NonTwoConsecutiveNonEmptyParse(chars: Set[Char]) extends BasicParser(chars) with NotTwoConsecutive[Char] with NonEmpty[Char]

class ShorterThanNParse(chars: Set[Char], override val max: Int) extends BasicParser(chars) with ShortenThanN[Char]

@main def checkParsers(): Unit =
  def parser = new BasicParser(Set('a', 'b', 'c'))
  println(parser.parseAll("aabc".toList)) // true
  println(parser.parseAll("aabcdc".toList)) // false
  println(parser.parseAll("".toList)) // true

  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  def parserNE = new NonEmptyParser(Set('0', '1'))
  println(parserNE.parseAll("0101".toList)) // true
  println(parserNE.parseAll("0123".toList)) // false
  println(parserNE.parseAll(List())) // false

  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  println(parserNTC.parseAll("XYZ".toList)) // true
  println(parserNTC.parseAll("XYYZ".toList)) // false
  println(parserNTC.parseAll("".toList)) // true

  // note we do not need a class name here, we use the structural type
  def parserNTCNE = new NonTwoConsecutiveNonEmptyParse(Set('X', 'Y', 'Z'))
  println(parserNTCNE.parseAll("XYZ".toList)) // true
  println(parserNTCNE.parseAll("XYYZ".toList)) // false
  println(parserNTCNE.parseAll("".toList)) // false

  def sparser: Parser[Char] = "abc".charParser // "abc".charParser()
  println(sparser.parseAll("aabc".toList)) // true
  println(sparser.parseAll("aabcdc".toList)) // false
  println(sparser.parseAll("".toList)) // true

  def shorterThanNSparser = new ShorterThanNParse(Set('a', 'b', 'c'), 4)
  println(shorterThanNSparser.parseAll("abc".toList))
  println(shorterThanNSparser.parseAll("ads".toList))
  println(shorterThanNSparser.parseAll("aaabbbbcccc".toList))
