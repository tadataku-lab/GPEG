import AST._

sealed trait Tree[T]
case class Leaf[T](name: Symbol, v: T) extends Tree[T]
case class Node[T](name: Symbol, left: Tree[T], right: Tree[T]) extends Tree[T]

object PegParser{

    var rules: Map[Symbol, PExp] = Map.empty[Symbol, PExp]

    def peg_parse(g: PGrammar, input: String): Option[(Tree[String], String)] = {
        rules = g.rules;
        val parser_context = new ParserContext(input, g.start)
        return exec(parser_context)
    }

    class ParserContext(in: String, start: PExp){
        val input = in
        var pos = 0
        var tree = null
        var exp = start
    }

    def exec(p: ParserContext): Option[(Tree[String], String)]={
        dispatch(p)
        return Some((p.tree, p.input.substring(p.pos)))
    }

    def dispatch(p: ParserContext):PExp = {
        p.exp match {
            case PMatch(bytes, next) => dispatch(pmatch(bytes,next,p))
            case PFail() => PFail()
            case PSucc() => PSucc()
            case PCall(symbol, next) => {
                rules.get(symbol) match {
                    case Some(exp) => {
                        p.exp = exp
                        if(dispatch(p) == PSucc()) {
                            p.exp = next
                            dispatch(p)
                        }else PFail()
                    }
                    case None => PFail()
                }
            }
            case PIf(lhs, rhs, next) => {
                val buf = p
                p.exp = lhs
                if(dispatch(p) == PSucc()){
                    p.exp = next
                    dispatch(p)
                }else{
                    buf.exp = rhs
                    if(dispatch(buf) == PSucc()){
                        buf.exp = next
                        dispatch(buf)
                    }else{
                        PFail()
                    }
                }
            }
        }
    }

    def pmatch(bytes: Array[Byte], next: PExp, p: ParserContext):ParserContext = {
        if(p.input.substring(p.pos, p.pos + bytes.length).getBytes == bytes){
            p.exp = next
            p.pos = p.pos + 1
            p
        }else{
            p.exp = PFail()
            p.pos = p.pos + 1
            p
        }
    }

}