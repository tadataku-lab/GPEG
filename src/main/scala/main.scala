import scala.util.parsing.combinator._
import scala.util.parsing.input.{CharSequenceReader, Position, StreamReader}
import java.io._
import Ast._
import PegParser._

object GpegParser{
    case class Pos(line: Int, column: Int)
    case class ParseException(pos: Pos, msg: String) extends Exception(pos + msg)

    object ParserCore extends Parsers {
        type Elem = Char;
        
        private val any: Parser[Char] = elem(".", c => c != CharSequenceReader.EofCh)
        private def chr(c: Char): Parser[Char] = c
        private def crange(f: Char, t: Char): Parser[Char] = elem("[]", c => f <= c && c <= t)
        lazy val GRAMMER: Parser[(Grammar, Pos)] = (loc <~ Spacing) ~ Definition.+ <~ EndOfFile ^^ {
            case pos ~ rules => ( Grammar( rules.head._2, rules.foldLeft(Map[Symbol,Exp]()){(m,r) => m.updated(r._1, r._2)}), Pos(pos.line, pos.column))
        }
        lazy val Definition: Parser[(Symbol,Exp)] = (Nonterminal <~ (LEFTARROW | EQ)) ~ Expression <~ SEMI_COLON ^^ {
            case n ~ b => (n._1.name, b._1)
        }
        lazy val Expression: Parser[(Exp, Pos)] = (
            Sequence ~ (BAR ~> Sequence).+ ^^ { case x ~ xs => xs.foldLeft(x){(a, y) => (Alt(a._1, y._1), y._2)}}
            | Sequence ~ (SLASH ~> Sequence).+ ^^ { case x ~ xs => xs.foldLeft(x){(a, y) => (Choice(a._1, y._1), y._2)}}
            | Sequence
        )
        lazy val Sequence: Parser[(Exp, Pos)] = Prefix.+ ^^ { case x::xs => 
            xs.foldLeft(x){(a, y) => (Seq(a._1, y._1), y._2)}
        }
        lazy val Prefix: Parser[(Exp, Pos)] = (
            (loc <~ AND) ~ Suffix ^^ { case pos ~ e =>  ( And(e._1), Pos(pos.line, pos.column)) }
            | (loc <~ NOT) ~ Suffix ^^ { case pos ~ e => ( Not(e._1), Pos(pos.line, pos.column)) }
            | Suffix
        )
        lazy val Suffix: Parser[(Exp, Pos)] = (
            loc ~ Primary <~ QUESTION ^^ { case pos ~ e => (Choice(e._1, Empty()), Pos(pos.line, pos.column)) }
            | loc ~ Primary <~ STAR ^^ { case pos ~ e => (Many(e._1), Pos(pos.line, pos.column)) }
            | loc ~ Primary <~ PLUS ^^ { case pos ~ e => (Seq(e._1, Many(e._1)), Pos(pos.line, pos.column)) }
            | Primary
        )
        lazy val Primary: Parser[(Exp, Pos)] = (
            Nonterminal 
            | OPEN ~> Expression <~ CLOSE
            | Literal
            | CLASS
            | loc <~ DOT ^^ { case pos => (Any(), Pos(pos.line, pos.column)) }
        )
        lazy val Literal: Parser[(Exp, Pos)] = loc ~ (
            chr('\'') ~> (not('\'') ~> CHAR).* <~ chr('\'') <~ Spacing
            | chr('"') ~> (not('"') ~> CHAR).* <~ chr('"') <~ Spacing
            ) ^^ {
            case pos ~ (c::cs) => (cs.foldLeft(AnyChar(c): Exp){(a, y) => Seq(a, AnyChar(y))}, Pos(pos.line, pos.column))
        }
        lazy val CLASS: Parser[(Exp, Pos)] = (loc <~ chr('[')) ~ (not(chr(']')) ~> Range).* <~ ']' ~> Spacing ^^ {
            case pos ~ (r::rs) => (rs.foldLeft(r){(a, y) => Seq(a, y)}, Pos(pos.line, pos.column));
        }
        lazy val Range: Parser[Exp] = (
            CHAR ~ '-' ~ CHAR ^^ { case f~_~t => (f to t).foldRight(AnyChar(t): Exp){(x, acc) => Seq(AnyChar(x), acc)} }
            | CHAR ^^ { case c => AnyChar(c) }
        )
        lazy val CHAR: Parser[Char] = ( 
            not('\\') ~ any ^^ { case _ ~ c => c}
        )
        lazy val Nonterminal: Parser[(NonTerm, Pos)] = loc ~ NonterminalStart ~ NonterminalCont.* <~ Spacing ^^ {
            case pos ~ s ~ c => ( NonTerm(Symbol("" + s + c.foldLeft("")(_ + _))), Pos(pos.line, pos.column))
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
        //val g = parse(new FileReader(args(0)))
        val g = parse(new FileReader("src/main/resources/GPEG/rule.gpeg"))
        println(g)
        val result = peg_parse(g,"1+1");
        println(result)
    }
    
    def parse(content: java.io.Reader):Grammar  = {
        ParserCore.GRAMMER(StreamReader(content)) match {
            case ParserCore.Success(node, _) => node._1
            case ParserCore.Failure(msg, rest) => 
                val pos = rest.pos
                throw new ParseException(Pos(pos.line, pos.column), msg)
            case ParserCore.Error(msg, rest) =>
                val pos = rest.pos
                throw new ParseException(Pos(pos.line, pos.column), msg)    
        }
    }
}
