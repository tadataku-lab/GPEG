import AST._
import scala.collection.mutable.{HashMap}

/**
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
*/

object PegPackratParser{

    var rules: Map[Symbol, PExp] = Map.empty[Symbol, PExp]

    def peg_parse(g: PGrammar, input: String): Option[(Tree, String)] = {
        rules = g.rules;
        val start = 
        rules.get(g.start) match {
            case Some(exp) => exp
            case None => throw new RuntimeException(g.start + ": Rule can not be found")
        }
        val parser_context = new ParserContext(input, 0, start, new HashMap[(Symbol, Int),Memo], g.start)
        return exec(g.start, parser_context)
    }

    class ParserContext(in: String, _pos: Int, start: PExp, _hash_table: HashMap[(Symbol, Int),Memo], startN: Symbol){
        val input = in
        var pos = _pos
        var exp = start
        var hash_table = _hash_table
        var nonterm = startN

        def copy(): ParserContext = {
            new ParserContext(input, pos, exp.copy, hash_table, nonterm)
        }
    }

    class Memo(_pos: Int, _exp: PExp, _nonterm: Symbol, _tree: List[Tree]){
        val pos = _pos
        val exp = _exp
        val nonterm = _nonterm
        val tree = _tree
        override def toString: String = {
            val sb = new StringBuilder
            sb.append("pos: " + pos + " tree: " + tree.toString)
            sb.toString
        }
    }

    def exec(start: Symbol, p: ParserContext): Option[(Tree, String)]={
        val (treeList, new_p) = memorized(List.empty[Tree], p)
        //println(p.hash_table)
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
                            val new_tree = tree:+Leaf(s)
                            parse(new_tree, p)
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
                    case PCall(symbol, next) => {
                        rules.get(symbol) match {
                    
                            case Some(exp) => {
                                p.exp = exp
                                p.nonterm = symbol
                                val (child_tree, new_p) = memorized(List.empty[Tree], p)
                                var new_tree = List.empty[Tree]
                                new_p.exp match {
                                    case PFail(_) => {
                                        // do nothing
                                    }
                                    case _ => {
                                        new_tree = tree:+Node(symbol,child_tree)
                                    }
                                } 
                                new_p.exp = next
                                //new_p.hash_table = p.hash_table
                                parse(new_tree, new_p)
                            }
                        
                            case None => throw new RuntimeException(symbol + ": Rule can not be found")
                        }
                    }
                
            
                    case PIf(lhs, rhs, next) => {
                        p.exp = lhs
                        val (lhs_tree, lhs_p) = parse(tree, p.copy)
                        p.hash_table = lhs_p.hash_table
                        lhs_p.exp match {
                            case PFail(_) => {
                                p.exp = rhs
                                val (rhs_tree, rhs_p) = parse(tree,p.copy)
                                p.hash_table = rhs_p.hash_table
                                rhs_p.exp match {
                                    case PFail(msg) => {
                                        p.exp = PFail(msg)
                                        (tree, p)
                                    }
                                    case _ => {
                                        rhs_p.exp = next
                                        (rhs_tree, rhs_p) 
                                    }
                                }
                            }
                            case _ => {
                                lhs_p.exp = next
                                (lhs_tree, lhs_p)
                            }
                        }
                    }

                    case PUnion(lhs, rhs) => {               
                        p.exp = lhs.copy
                        val (lhs_tree, lhs_p) = memorized(tree, p.copy)
                        lhs_p.exp match {
                            case PFail(_) => {
                                p.exp = rhs.copy
                                val (rhs_tree, rhs_p) = memorized(tree,p.copy)
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
                                val (rhs_tree, rhs_p) = memorized(tree,p.copy)
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
                        val (new_tree, new_p) = memorized(tree, p)
                        new_p.exp match {
                            case PFail(_) => {
                                p.exp = next
                                memorized(tree, p)
                            } 
                            case _ => {
                                p.exp = PFail("Match PExp: " + body)
                                memorized(tree, p)
                            }
                        }
                    }

                    case PAnd(body, next) => {
                        p.exp = body
                        val (new_tree, new_p) = memorized(tree, p)
                        new_p.exp match {
                            case PFail(_) => {
                                p.exp = PFail("Dont't match PExp: " + body)
                                memorized(tree, p)
                            } 
                            case _ => {
                                p.exp = next
                                memorized(tree, p)
                            }
                        }
                    }

                    case PMany(body, next) => {
                        p.exp = body
                        val (new_tree, new_p) = many(tree, p)
                        new_p.exp = next
                        (new_tree, new_p)
                    }

                }
    }        


    def many(tree: List[Tree], p: ParserContext): (List[Tree], ParserContext) = {
        val body = p.exp
        val (new_tree, new_p) = memorized(tree,p)
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

    def memorized(tree: List[Tree], p: ParserContext):(List[Tree], ParserContext) = {
        p.hash_table.get((p.nonterm,p.pos)) match {
            case Some(memo) => {
                //println(p.pos + " " + p.nonterm)
                //println(memo.pos + " " + memo.nonterm + " " + memo.exp)
                p.pos = memo.pos
                p.nonterm = memo.nonterm
                p.exp = memo.exp
                /**
                var new_tree = List.empty[Tree]
                memo_tree match {
                    case null => //do nothing
                    case _ => {
                        new_tree = tree:+Node(p.nonterm,memo_tree)
                    }
                }
                */
                (memo.tree, p)
            }
            case None => {
                val memo_info = (p.nonterm, p.pos)
                val (new_tree, new_p) = parse(tree, p)
                new_p.exp match {
                    case PFail(_) => {
                        //p.hash_table += ((memo_info) -> new Memo(new_p.pos, new_p.exp, new_p.nonterm, new_tree))
                    }
                    case _ => {
                        p.hash_table += ((memo_info) -> new Memo(new_p.pos, new_p.exp, new_p.nonterm, new_tree))
                    }
                }
                //p.hash_table += ((memo_info) -> new Memo(new_p.pos, new_p.exp, new_p.nonterm, new_tree))
                //println(p.hash_table)
                (new_tree, new_p)
            }
        }
    }

}