import AST._
import scala.collection.mutable.{HashMap}

sealed trait Tree
case class Leaf(v: String) extends Tree{
    override def toString: String = {
      "[" + v + "]" 
    }
}
case class Node(name: Symbol, next: List[Tree]) extends Tree{
    override def toString: String = {
        var sb = new StringBuilder
        sb.append("[" + name + " ")
        for (tree <- next){
            sb.append(tree)
        }
        sb.append("]")
        sb.toString
    }
}

object PegParser{

    var rules: Map[Symbol, PExp] = Map.empty[Symbol, PExp]

    def peg_parse(g: PGrammar, input: String): Option[(Tree, String)] = {
        rules = g.rules;
        val start = 
        rules.get(g.start) match {
            case Some(exp) => exp
            case None => throw new RuntimeException(g.start + ": Rule can not be found")
        }
        val parser_context = new ParserContext(input, 0, start)
        return exec(g.start, parser_context)
    }

    class ParserContext(in: String, _pos: Int, start: PExp){
        val input = in
        var pos = _pos
        var exp = start
        var hash_table = new HashMap[(Symbol, Int),(List[Tree], Int)]

        def copy(): ParserContext = {
            new ParserContext(input, pos, exp.copy)
        }
    }

    def exec(start: Symbol, p: ParserContext): Option[(Tree, String)]={
        val (treeList, new_p) = parse(List.empty[Tree], p)
        return Some((Node(start, treeList), new_p.input.substring(new_p.pos)))
    }

    def parse(tree: List[Tree], p: ParserContext):(List[Tree], ParserContext) = {
        p.exp match {
            case PSucc() => {
                return (tree, p)
            }
            case PFail(msg) => throw new RuntimeException(msg)
            
            case PMatch(bytes, next) => {
                if((bytes.length + p.pos) > p.input.length){
                    p.exp = PFail("String index out of range:" + (bytes.length + p.pos))
                    return (tree, p)
                }
                val s = p.input.substring(p.pos, p.pos + bytes.length)
                if(bytesEq(bytes,s)){
                    p.exp = next
                    p.pos = p.pos + bytes.length
                    if((bytes.length + p.pos) == p.input.length){
                        val new_tree = tree:+Leaf(s)
                        (new_tree, p)
                    }else{
                        val new_tree = tree:+Leaf(s)
                        parse(new_tree, p)
                    }
                }else {
                    p.exp = PFail("pos: " + (p.pos + 1) + " string: "+ s + " -> don't match " + (bytes.map(_.toChar)).mkString)
                    (tree, p)
                }
            }
            case PAny(next) => {
                if((1 + p.pos) > p.input.length){
                    p.exp = PFail("String index out of range:" + (1 + p.pos))
                    return (tree, p)
                }
                val s = p.input.substring(p.pos, p.pos + 1)
                p.exp = next
                p.pos = p.pos + 1
                val new_tree = tree:+Leaf(s)
                parse(new_tree, p)        
            }
            /**
            case PCall(symbol, next) => {
                rules.get(symbol) match {
                    case Some(exp) => {
                        p.exp = exp
                        val result = parse(List.empty[Tree], p)
                        var new_tree = List.empty[Tree]
                        p.exp match {
                            case PFail(_) => //do nothing
                            case _ => {new_tree = tree:+Node(symbol,result._1)}
                        }
                        p.exp = next
                        p.pos = result._2.pos
                        parse(new_tree, p)
                    }
                    case None => throw new RuntimeException(symbol + ": Rule can not be found")
                }
            }
            */
            
            case PCall(symbol, next) => {
                p.hash_table.get((symbol,p.pos)) match {
                    case Some((memo_tree, memo_pos)) => {
                        println("memo")
                        p.exp = next
                        p.pos = memo_pos
                        var new_tree = List.empty[Tree]
                        memo_tree match {
                            case null => //do nothing
                            case _ => {new_tree = tree:+Node(symbol,memo_tree)}
                        }
                        parse(new_tree, p)
                    }
                    case None => {
                        rules.get(symbol) match {
                            case Some(exp) => {
                                val memo_pos = p.pos
                                p.exp = exp
                                val result = parse(List.empty[Tree], p)
                                var new_tree = List.empty[Tree]
                                p.exp match {
                                    case PFail(_) => {
                                        p.hash_table += ((symbol,memo_pos) -> (null, result._2.pos))
                                    }
                                    case _ => {
                                        p.hash_table += ((symbol,memo_pos) -> (result._1, result._2.pos))
                                        new_tree = tree:+Node(symbol,result._1)
                                    }
                                } 
                                p.exp = next
                                p.pos = result._2.pos
                                p.hash_table = result._2.hash_table
                                parse(new_tree, p)
                            }
                            case None => throw new RuntimeException(symbol + ": Rule can not be found")
                        }
                    }
                }
            }
            
            case PIf(lhs, rhs, next) => {
                p.exp = lhs
                val (lhs_tree, lhs_p) = parse(tree, p.copy)
                lhs_p.exp match {
                    case PFail(_) => {
                        p.exp = rhs
                        val (rhs_tree, rhs_p) = parse(tree,p.copy)
                        rhs_p.exp match {
                            case PFail(msg) => {
                                p.exp = PFail(msg)
                                (tree, p)
                            }
                            case _ => {
                                rhs_p.exp = next
                                parse(rhs_tree, rhs_p) 
                            }
                        }
                    }
                    case _ => {
                        lhs_p.exp = next
                        parse(lhs_tree, lhs_p)
                    }
                }
            }

            case PUnion(lhs, rhs) => {               
                p.exp = lhs.copy
                val (lhs_tree, lhs_p) = parse(tree, p.copy)
                lhs_p.exp match {
                    case PFail(_) => {
                        p.exp = rhs.copy
                        val (rhs_tree, rhs_p) = parse(tree,p.copy)
                        rhs_p.exp match {
                            case PFail(msg) => {                                                              
                                p.exp = PFail(msg)
                                (tree, p)
                            }
                            case _ => {
                                (rhs_tree, rhs_p) 
                            }
                        }
                    }
                    case _ => {
                        p.exp = rhs.copy
                        val (rhs_tree, rhs_p) = parse(tree,p.copy)
                        rhs_p.exp match {
                            case PFail(_) => {
                                (lhs_tree, lhs_p)
                            }
                            case _ => {
                                if(lhs_p.pos == rhs_p.pos){
                                    val new_tree = tree:+Node(Symbol("ambiguity"), List(Node(Symbol("lhs"),lhs_tree), Node(Symbol("rhs"),rhs_tree)))
                                    (new_tree, lhs_p)
                                }else throw new RuntimeException("Don't hold confluence")
                            }
                        }
                    }
                }
            }
            
            case PNot(body, next) => {
                p.exp = body
                val (new_tree, new_p) = parse(tree, p)
                new_p.exp match {
                    case PFail(_) => {
                        p.exp = next
                        parse(tree, p)
                    } 
                    case _ => {
                        p.exp = PFail("Match PExp: " + body)
                        parse(tree, p)
                    }
                }
            }

            case PAnd(body, next) => {
                p.exp = body
                val (new_tree, new_p) = parse(tree, p)
                new_p.exp match {
                    case PFail(_) => {
                        p.exp = PFail("Dont't match PExp: " + body)
                        parse(tree, p)
                    } 
                    case _ => {
                        p.exp = next
                        parse(tree, p)
                    }
                }
            }

            case PMany(body, next) => {
                p.exp = body
                val (new_tree, new_p) = many(tree, p)
                new_p.exp = next
                parse(new_tree, new_p)
            }

        }
    }

    def many(tree: List[Tree], p: ParserContext): (List[Tree], ParserContext) = {
        val body = p.exp
        val (new_tree, new_p) = parse(tree,p)
        p.exp match {
            case PFail(_) => return (tree, p)
            case _ => {
                new_p.exp = body
                many(new_tree, new_p)
            }
        }
    }

    def bytesEq(bytes: Array[Byte], string: String):Boolean = {
        if(bytes.length == string.length){
            bytes.sameElements(string.getBytes)
        }else throw new Exception("don't match length")
    }

    /**

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

    */

}