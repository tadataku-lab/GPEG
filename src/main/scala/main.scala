import scala.util.parsing.combinator._
import scala.util.parsing.input.{CharSequenceReader, Position}
import Ast._

case class MyData( val pos: Pos,val d: Char, val a: Char ) {}

object OpegParser{
    case class ParseException(pos: Pos, msg: String) extends Exception(pos + msg)

    object ParserCore extends Parsers {
        type Elem = Char;
        lazy val loc: Parser[Position] = Parser{reader => Success(reader.pos, reader)} 
        val isDigit = elem( "DIGIT", { case c => '0' <= c && c <= '9' } );
        val isAlpha = elem( "ALPHA", { case c => 'a' <= c && c <= 'z' } );
        val parser: Parser[ MyData ] = (loc) ~ isDigit ~ isAlpha ^^ { case pos ~ d ~ a => MyData(Pos(pos.line, pos.column), d, a ) };
    }

    def main(args: Array[String]) = {
        val g = parse("5a")
        println(g)
        val e = parse("5A")
        println(e)
    }
    
    def parse(doc: String):MyData  = {
        ParserCore.parser(new CharSequenceReader( doc )) match {
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
