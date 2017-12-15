import AST._

sealed trait Tree
case class Leaf(v: String) extends Tree
case class Node(name: Symbol, next: List[Tree]) extends Tree

object PegParser{

    var rules: Map[Symbol, PExp] = Map.empty[Symbol, PExp]

    def peg_parse(g: PGrammar, input: String): Option[(Tree, String)] = {
        rules = g.rules;
        val start = 
        rules.get(g.start) match {
            case Some(exp) => exp
            case None => throw new RuntimeException(g.start + ": Rule can not be found")
        }
        val parser_context = new ParserContext(input, start)
        return exec(g.start, parser_context)
    }

    class ParserContext(in: String, start: PExp){
        val input = in
        var pos = 0
        var tree = null
        var exp = start
    }

    def exec(start: Symbol, p: ParserContext): Option[(Tree, String)]={
        dispatch(Node(start,null), p)
        return Some((p.tree, p.input.substring(p.pos)))
    }

    def parse(tree: List[Tree], p: ParserContext):(List[Tree], ParserContext) = {
        p.exp match {
            case PMatch(bytes, next) => {
                val s = p.input.substring(p.pos, p.pos + bytes.length)
                if(bytesEq(bytes,s)){
                    p.exp = next
                    p.pos = p.pos + bytes.length
                    tree:+Leaf(s)
                    parse(tree, p)
                }else throw new RuntimeException(s + ": don't match " + (bytes.map(_.toChar)).mkString)
            }
            case PCall(symbol, next) => {
                rules.get(symbol) match {
                    case Some(exp) => {
                        p.exp = exp
                        val result = parse(tree, p)
                        p.exp = next
                        tree:+Node(symbol,result._1)
                        parse(tree, p)
                    }
                    case None => throw new RuntimeException(symbol + ": Rule can not be found")
                }
            }
        }
    }

    def bytesEq(bytes: Array[Byte], string: String):Boolean = {
        if(bytes.length == string.length){
            bytes.sameElements(string.getBytes)
        }else throw new Exception("don't match length")
    }

    def dispatch(tree: Tree, p: ParserContext):PExp = {
        p.exp match {
            case PMatch(bytes, next) => dispatch(tree, pmatch(bytes,next,p))
            case PFail() => PFail()
            case PSucc() => PSucc()
            case PCall(symbol, next) => {
                rules.get(symbol) match {
                    case Some(exp) => {
                        p.exp = exp
                        if(dispatch(tree, p) == PSucc()) {
                            p.exp = next
                            dispatch(tree, p)
                        }else PFail()
                    }
                    case None => PFail()
                }
            }
            case PIf(lhs, rhs, next) => {
                val buf = p
                p.exp = lhs
                if(dispatch(tree, p) == PSucc()){
                    p.exp = next
                    dispatch(tree, p)
                }else{
                    buf.exp = rhs
                    if(dispatch(tree, buf) == PSucc()){
                        buf.exp = next
                        dispatch(tree, buf)
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