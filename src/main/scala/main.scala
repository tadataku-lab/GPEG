import scala.util.parsing.combinator._
import scala.util.parsing.input.{CharSequenceReader, Position, StreamReader}
import java.io._
import Ast._
import PegParser._
import scala.io.Source

object GpegParser{
    case class Pos(line: Int, column: Int)
    case class ParseException(pos: Pos, msg: String) extends Exception(pos + msg)

    object ParserCore extends Parsers {
        type Elem = Char;
        
        private val any: Parser[Char] = elem(".", c => c != CharSequenceReader.EofCh)
        private def chr(c: Char): Parser[Char] = c
        private def crange(f: Char, t: Char): Parser[Char] = elem("[]", c => f <= c && c <= t)
        lazy val GRAMMER: Parser[Grammar] = (loc <~ Spacing) ~ Definition.+ <~ EndOfFile ^^ {
            case pos ~ rules =>  Grammar( rules.head._2, rules.foldLeft(Map[Symbol,Exp]()){(m,r) => m.updated(r._1, r._2)})
        }
        lazy val Definition: Parser[(Symbol,Exp)] = (Nonterminal <~ (LEFTARROW | EQ)) ~ Expression <~ SEMI_COLON ^^ {
            case n ~ b => (n.name, b)
        }
        lazy val Expression: Parser[Exp] = (
            Sequence ~ (BAR ~> Sequence).+ ^^ { case x ~ xs => xs.foldLeft(x){(a, y) => Alt(a, y)}}
            | Sequence ~ (SLASH ~> Sequence).+ ^^ { case x ~ xs => xs.foldLeft(x){(a, y) => Choice(a, y)}}
            | Sequence
        )
        lazy val Sequence: Parser[Exp] = Prefix.+ ^^ { case x::xs => 
            xs.foldLeft(x){(a, y) => Seq(a, y)}
        }
        lazy val Prefix: Parser[Exp] = (
            (loc <~ AND) ~ Suffix ^^ { case pos ~ e =>   And(e) }
            | (loc <~ NOT) ~ Suffix ^^ { case pos ~ e =>  Not(e) }
            | Suffix
        )
        lazy val Suffix: Parser[Exp] = (
            loc ~ Primary <~ QUESTION ^^ { case pos ~ e => Choice(e, Empty()) }
            | loc ~ Primary <~ STAR ^^ { case pos ~ e => Many(e) }
            | loc ~ Primary <~ PLUS ^^ { case pos ~ e => Seq(e, Many(e)) }
            | Primary
        )
        lazy val Primary: Parser[Exp] = (
            Nonterminal 
            | OPEN ~> Expression <~ CLOSE
            | Literal
            | CLASS
            | loc <~ DOT ^^ { case pos => Any() }
        )
        lazy val Literal: Parser[Exp] = loc ~ (
            chr('\'') ~> (not('\'') ~> CHAR).* <~ chr('\'') <~ Spacing
            | chr('"') ~> (not('"') ~> CHAR).* <~ chr('"') <~ Spacing
            ) ^^ {
            case pos ~ (c::cs) => cs.foldLeft(AnyChar(c): Exp){(a, y) => Seq(a, AnyChar(y))}
        }
        lazy val CLASS: Parser[Exp] = (loc <~ chr('[')) ~ (not(chr(']')) ~> Range).* <~ ']' ~> Spacing ^^ {
            case pos ~ (r::rs) => rs.foldLeft(r){(a, y) => Seq(a, y)}
        }
        lazy val Range: Parser[Exp] = (
            CHAR ~ '-' ~ CHAR ^^ { case f~_~t => (f to t).foldRight(AnyChar(t): Exp){(x, acc) => Seq(AnyChar(x), acc)} }
            | CHAR ^^ { case c => AnyChar(c) }
        )
        lazy val CHAR: Parser[Char] = ( 
            not('\\') ~ any ^^ { case _ ~ c => c}
        )
        lazy val Nonterminal: Parser[NonTerm] = loc ~ NonterminalStart ~ NonterminalCont.* <~ Spacing ^^ {
            case pos ~ s ~ c =>  NonTerm(Symbol("" + s + c.foldLeft("")(_ + _)))
        }
        lazy val NonterminalStart: Parser[Char] = crange('a','z') | crange('A','Z') | '_'
        lazy val NonterminalCont: Parser[Char] = NonterminalStart | crange('0','9')
        lazy val loc: Parser[Position] = Parser{reader => Success(reader.pos, reader)}
        lazy val SEMI_COLON = ';' <~ Spacing
        lazy val LEFTARROW = chr('<') ~ '-' <~ Spacing
        lazy val EQ = chr('=') <~ Spacing
        lazy val SLASH = '/' <~ Spacing
        lazy val BAR = '|' <~ Spacing
        lazy val AND = '&' <~ Spacing
        lazy val NOT = '!' <~ Spacing
        lazy val QUESTION = '?' <~ Spacing
        lazy val STAR = '*' <~ Spacing
        lazy val PLUS = '+' <~ Spacing
        lazy val OPEN = '(' <~ Spacing
        lazy val CLOSE = ')' <~ Spacing
        lazy val DOT = '.' <~ Spacing
        lazy val Spacing = (Space | Comment).*
        lazy val Comment = chr('#') ~ (not(EndOfLine) ~ any).* ~ EndOfLine
        lazy val Space = chr(' ') | chr('\t') | EndOfLine
        lazy val EndOfLine = chr('\r') ~ chr('\n') | chr('\n') | chr('\r')
        lazy val EndOfFile = not(any)
    }

    def main(args: Array[String]) = {
        //val file = new PrintWriter(args(0))
        val g = parse(new FileReader("src/main/resources/GPEG/rule.gpeg"))
        println(g);
        //file.write(g.toString())
        //file.close()
        val result = peg_parse(g,"1+1");
        println(result)
    }

    def file2string(filename: String): String = {
        val source = Source.fromFile(filename)
        val sb = new StringBuilder
        for( line <- source.getLines ) {
            sb.append(line)
        }
        source.close
        return sb.toString
    }

    def parse(content: java.io.Reader):Grammar  = {
        ParserCore.GRAMMER(StreamReader(content)) match {
            case ParserCore.Success(node, _) => node
            case ParserCore.Failure(msg, rest) => 
                val pos = rest.pos
                throw new ParseException(Pos(pos.line, pos.column), msg)
            case ParserCore.Error(msg, rest) =>
                val pos = rest.pos
                throw new ParseException(Pos(pos.line, pos.column), msg)    
        }
    }
}
