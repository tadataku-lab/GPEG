import scala.util.parsing.combinator._

case class WordFreq(word: String, count: Int) {
    override def toString = "Word <" + word + "> " +
                            "occurs with frequency " + count
}

class SimpleParser extends RegexParsers {
    def word: Parser[String]   = """[a-z]+""".r       ^^ { _.toString }
    def number: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }
    def freq: Parser[WordFreq] = word ~ number        ^^ { case wd ~ fr => WordFreq(wd,fr) }
}

object TestSimpleParser extends SimpleParser {
    def main(args: Array[String]) = {
        val g = parse("johnny 121")
        println(g)
    }
    
    def parse(doc: String): WordFreq = {
        parse(freq, doc) match {
            case Success(matched,_) => matched
            case Failure(msg, _) => 
                throw new Exception(msg)
            case Error(msg, _) =>
                throw new Exception(msg)
        }
    }
}