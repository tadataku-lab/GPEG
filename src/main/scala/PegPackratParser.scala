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
    var input: Array[Byte] = Array.empty[Byte]

    def peg_parse(g: PGrammar, _input: String): Option[(Tree, String)] = {
        rules = g.rules;
        val start = 
        rules.get(g.start) match {
            case Some(exp) => exp
            case None => throw new RuntimeException(g.start + ": Rule can not be found")
        }
        input = _input.getBytes
        val parser_context = new ParserContext(0, start, new HashMap[(Symbol, Int),Memo], g.start)
        return exec(g.start, parser_context)
    }

    class ParserContext(_pos: Int, start: PExp, _hash_table: HashMap[(Symbol, Int),Memo], startN: Symbol){
        var pos = _pos
        var exp = start
        var hash_table = _hash_table
        var nonterm = startN

        def copy(): ParserContext = {
            new ParserContext(pos, exp.copy, hash_table, nonterm)
        }
    }

    class Memo(_pos: Int, _tree: List[Tree]){
        val pos = _pos
        val tree = _tree
        override def toString: String = {
            val sb = new StringBuilder
            sb.append("pos: " + pos + " tree: " + tree.toString)
            sb.toString
        }
    }

    def exec(start: Symbol, p: ParserContext): Option[(Tree, String)]={
        val (treeList, new_p) = parse(List.empty[Tree], p)
        //println(new_p.hash_table)
        return Some((Node(start, treeList), (input.drop(new_p.head.pos)).map(_.toChar).mkString))
    }

    def parse(tree: List[Tree], p: ParserContext):(List[Tree], List[ParserContext]) = {
               p.exp match {
                    case PSucc() => {
                        return (tree, List(p))
                    }
                    case PFail(msg) => throw new RuntimeException(msg)
            
                    case PMatch(bytes, next) => {
                        val in = input
                        val bytes_length = bytes.length
                        if((bytes_length + p.pos) > in.length){
                            p.exp = PFail("")//String index out of range:" + (bytes.length + p.pos))
                            return (tree, List(p))
                        }
                        if(bytesEq(bytes, p.pos, bytes_length)){
                            p.exp = next
                            p.pos = p.pos + bytes_length
                            val new_tree = tree:+Leaf((bytes.map(_.toChar)).mkString)
                            parse(new_tree, p)
                        }else {
                            if((bytes.map(_.toChar)).mkString == ""){ // case Empty
                                p.exp = next
                                //val new_tree = tree:+Leaf("")
                                return parse(tree, p)
                            }
                            p.exp = PFail("")//"pos: " + (p.pos + 1) + " string: "+ s + " -> don't match " + (bytes.map(_.toChar)).mkString)
                            (tree, List(p))
                        }
                    }
                    case PAny(next) => {
                        if((p.pos + 1) > input.length){
                            p.exp = PFail("String index out of range:" + (1 + p.pos))
                            return (tree, List(p))
                        }
                        p.exp = next
                        p.pos = p.pos + 1
                        val new_tree = tree:+Leaf((input(p.pos).toChar).toString)
                        parse(new_tree, p)        
                    }
                    case PCall(symbol, next) => {
                        rules.get(symbol) match {
                    
                            case Some(exp) => {
                                p.exp = exp
                                p.nonterm = symbol
                                val (child_tree, new_p) = memorized(List.empty[Tree], p)
                                var new_tree = List.empty[Tree]
                                new_p.head.exp match {
                                    case PFail(_) => {
                                        p.exp = new_p.head.exp
                                        (tree, List(p))
                                    }
                                    case _ => {
                                        new_tree = tree:+Node(symbol,child_tree)
                                        new_p.head.exp = next
                                        parse(new_tree, new_p.head)
                                    }
                                } 
                            }
                        
                            case None => throw new RuntimeException(symbol + ": Rule can not be found")
                        }
                    }
                
            
                    case PIf(lhs, rhs, next) => {
                        p.exp = lhs
                        val (lhs_tree, lhs_p) = parse(tree, p.copy)
                        lhs_p.head.exp match {
                            case PFail(_) => {
                                p.exp = rhs
                                val (rhs_tree, rhs_p) = parse(tree,p.copy)
                                rhs_p.head.exp match {
                                    case PFail(msg) => {
                                        p.exp = PFail(msg)
                                        (tree, List(p))
                                    }
                                    case _ => {
                                        rhs_p.head.exp = next
                                        parse(rhs_tree, rhs_p.head) 
                                    }
                                }
                            }
                            case _ => {
                                lhs_p.head.exp = next
                                parse(lhs_tree, lhs_p.head)
                            }
                        }
                    }

                    case PUnion(lhs, rhs) => {         
                        p.exp = lhs
                        val (lhs_tree, lhs_p) = parse(tree, p.copy)
                        lhs_p.head.exp match {
                            case PFail(_) => {
                                p.exp = rhs
                                val (rhs_tree, rhs_p) = parse(tree,p.copy)
                                rhs_p.head.exp match {
                                    case PFail(msg) => {                                                              
                                        p.exp = PFail(msg)
                                        (tree, List(p))
                                    }
                                    case _ => {
                                        (rhs_tree, rhs_p) 
                                    }
                                }
                            }
                            case _ => {
                                p.exp = rhs
                                val (rhs_tree, rhs_p) = parse(tree,p.copy)
                                rhs_p.head.exp match {
                                    case PFail(_) => {
                                        (lhs_tree, lhs_p)
                                    }
                                    case _ => {
                                        if(lhs_p.head.pos == rhs_p.head.pos){
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
                        val (new_tree, new_p) = parse(tree, p.copy)
                        new_p.head.exp match {
                            case PFail(_) => {
                                p.exp = next
                                parse(tree, p)
                            } 
                            case _ => {
                                p.exp = PFail("Match PExp: " + body)
                                (tree, List(p))
                            }
                        }
                    }

                    case PAnd(body, next) => {
                        p.exp = body
                        val (new_tree, new_p) = parse(tree, p.copy)
                        new_p.head.exp match {
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

/**
    def many(tree: List[Tree], p: ParserContext): (List[Tree], ParserContext) = {
        
        val body = p.exp
        val (new_tree, new_p) = parse(tree,p.copy)
        new_p.exp match {
            case PFail(_) => {
                println("fail")
                return (tree, p)
            }
            case _ => {
                new_p.exp = body
                many(new_tree, new_p)
            }
        }
    }
*/
    def many(tree: List[Tree], p: ParserContext): (List[Tree], ParserContext) = {
        var result = true
        var (now_tree, now_p) = (tree,p)

        while(result){
            val body = now_p.exp
            val (new_tree, new_p) = parse(now_tree,now_p.copy)
            new_p.head.exp match {
                case PFail(_) => {
                    result = false
                }
                case _ => {
                    new_p.head.exp = body
                    now_tree = new_tree
                    now_p = new_p.head
                }
            }
        }
        
        (now_tree, now_p)
    }

    def bytesEq(bytes: Array[Byte], pos: Int, length: Int):Boolean = {
        if(bytes.length == length){
            /**
            for(i <- 0 until length){
                if(bytes(i) != input(pos + i)) return false
            }
            true
            */
            bytes.sameElements(input.slice(pos, pos + length))
        }else throw new Exception("don't match length")
    }

    def memorized(tree: List[Tree], p: ParserContext):(List[Tree], List[ParserContext]) = {
        p.hash_table.get((p.nonterm,p.pos)) match {
            case Some(memo) => {
                //println(p.pos + " " + p.nonterm)
                //println(memo.pos + " " + memo.nonterm + " " + memo.exp)
                p.pos = memo.pos
                //p.nonterm = memo.nonterm
                //p.exp = memo.exp
                (memo.tree, List(p))
            }
            case None => {
                val memo_info = (p.nonterm, p.pos)
                val (new_tree, new_p) = parse(tree, p)
                new_p.head.exp match {
                    case PFail(_) => {
                        //p.hash_table += ((memo_info) -> new Memo(new_p.pos, new_p.exp, new_p.nonterm, new_tree))
                    }
                    case _ => {
                        p.hash_table += ((memo_info) -> new Memo(new_p.head.pos, new_tree))
                    }
                }
                //p.hash_table += ((memo_info) -> new Memo(new_p.pos, new_p.exp, new_p.nonterm, new_tree))
                //println(p.hash_table)
                (new_tree, new_p)
            }
        }
    }

}