import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader
import Ast._

case class MyData( val d: Char, val a: Char ) {}

object OpegParser{
    object ParserCore extends Parsers {
        type Elem = Char;
        val isDigit = elem( "DIGIT", { case c => '0' <= c && c <= '9' } );
        val isAlpha = elem( "ALPHA", { case c => 'a' <= c && c <= 'z' } );
        val parser: Parser[ MyData ] = isDigit ~ isAlpha ^^ { case d ~ a => MyData( d, a ) };
    }

    def main(args: Array[String]) = {
        val g = parse("5a")
        println(g)
    }
    
    def parse(doc: String):MyData  = {
        ParserCore.parser(new CharSequenceReader( doc )) match {
            case ParserCore.Success(node, _) => node
            case ParserCore.Failure(msg, _) => 
                throw new Exception
            case ParserCore.Error(msg, _) =>
                throw new Exception     
        }
    }
}
