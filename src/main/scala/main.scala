import scala.util.parsing.combinator._
import scala.util.parsing.input.{CharSequenceReader, Position}
import Ast._

case class MyData( val pos: Pos,val d: Char, val a: Char ) {}

object OpegParser{
    case class ParseException(pos: Pos, msg: String) extends Exception(pos + msg)

    object ParserCore extends Parsers {
        type Elem = Char;
        private val any: Parser[Char] = elem(".", c => c != CharSequenceReader.EofCh)
        private def chr(c: Char): Parser[Char] = c
        private def crange(f: Char, t: Char): Parser[Char] = elem("[]", c => f <= c && c <= t)
        lazy val GRAMMER: Parser[Grammar] = (loc <~ Spacing) ~ Definition.+ <~ EndOfFile ^^ {
            case pos ~ rules => Grammar(Pos(pos.line, pos.column), rules.head.name, rules)
        }
        lazy val Definition: Parser[Rule] = (Nonterminal <~ (LEFTARROW | EQ)) ~ Expression <~ Spacing ^^ {
            case n ~ b => Rule(n.pos, n.name, b)
        }
        lazy val Expression: Parser[Exp] = Sequence ~ (SLASH ~> Sequence).* ^^ {
            case x ~ xs => xs.foldLeft(x){(a, y) => Choice(y.pos, a, y)}
        }
        lazy val Sequence: Parser[Exp] = Prefix.+ ^^ { case x::xs => 
            xs.foldLeft(x){(a, y) => Seq(y.pos, a, y)}
        }
        lazy val Prefix: Parser[Exp] = (
            (loc <~ AND) ~ Suffix ^^ { case pos ~ e =>   Not(Pos(pos.line, pos.column),Not(Pos(pos.line, pos.column), e)) }
            | (loc <~ NOT) ~ Suffix ^^ { case pos ~ e => Not(Pos(pos.line, pos.column), e) }
            | Suffix
        )
        lazy val Suffix: Parser[Exp] = (
            loc ~ Primary <~ QUESTION ^^ { case pos ~ e => Choice(Pos(pos.line, pos.column), e, Empty(Pos(pos.line, pos.column), Unit)) }
            | loc ~ Primary <~ STAR ^^ { case pos ~ e => Rep(Pos(pos.line, pos.column), e) }
            | loc ~ Primary <~ PLUS ^^ { case pos ~ e => Seq(Pos(pos.line, pos.column), e, Rep(Pos(pos.line, pos.column), e)) }
            | Primary
        )
        lazy val Primary: Parser[Exp] = (
            Nonterminal 
            | OPEN ~> Expression <~ CLOSE
            | Literal
            | loc <~ DOT ^^ { case pos => Wildcard(Pos(pos.line, pos.column)) }
        )
        lazy val Literal: Parser[Str] = loc ~ (
            chr('\'') ~> (not('\'') ~> CHAR).* <~ chr('\'') <~ Spacing
            | chr('"') ~> (not('"') ~> CHAR).* <~ chr('"') <~ Spacing
            ) ^^ {
            case pos ~ cs => Str(Pos(pos.line, pos.column), cs.foldLeft(""){(acc, n) => acc + n})
        }
        lazy val CHAR: Parser[Char] = ( 
            not('\\') ~ any ^^ { case _ ~ c => c}
        )
        lazy val Nonterminal: Parser[AnyNonterminal] = loc ~ NonterminalStart ~ NonterminalCont.* <~ Spacing ^^ {
            case pos ~ s ~ c => AnyNonterminal(Pos(pos.line, pos.column), Symbol("" + s + c.foldLeft("")(_ + _)))
        }
        lazy val NonterminalStart: Parser[Char] = crange('a','z') | crange('A','Z') | '_'
        lazy val NonterminalCont: Parser[Char] = NonterminalStart | crange('0','9')
        lazy val loc: Parser[Position] = Parser{reader => Success(reader.pos, reader)} 
        lazy val LEFTARROW = chr('<') ~ '-' <~ Spacing
        lazy val EQ = chr('=') <~ Spacing
        lazy val SLASH = '/' <~ Spacing
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
        val g = parse("A = B")
        println(g)
    }
    
    def parse(doc: String):Grammar  = {
        ParserCore.GRAMMER(new CharSequenceReader( doc )) match {
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
