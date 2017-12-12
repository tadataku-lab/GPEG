import Ast._

sealed trait Tree[T]
case class Leaf[T](name: Symbol, v: T) extends Tree[T]
case class Node[T](name: Symbol, left: Tree[T], right: Tree[T]) extends Tree[T]

object PegParser{
    def peg_parse(g: Grammar, input: String): Option[(Tree[String], String)] = {
        var parser_context = new ParserContext(g, input)
        return Some((parser_context.tree, parser_context.input.substring(parser_context.pos)))
    }

    class ParserContext(g: Grammar, in: String){
        val start = g.start
        val rules = g.rules
        val input = in
        var pos = 0
        var tree = null
    }

}