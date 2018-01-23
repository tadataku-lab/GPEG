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
    var id: Int = 0
    var lrbs: Map[Int, LorRorB] = Map.empty[Int,LorRorB]

    def peg_parse(g: PGrammar, _input: String): Option[(Tree, ContextTree)] = {
        rules = g.rules;
        val start = 
        rules.get(g.start) match {
            case Some(exp) => exp
            case None => throw new RuntimeException(g.start + ": Rule can not be found")
        }
        input = _input.getBytes
        val parser_context = new ParserContext(0, start, new HashMap[(Symbol, Int),Memo], g.start, false)
        return exec(g.start, parser_context)
    }

    sealed trait LorRorB

    case class Left() extends LorRorB
    case class Right() extends LorRorB
    case class Both() extends LorRorB

    sealed trait ContextTree {
        def setExp(body: PExp): ContextTree = {
            this match {
                case a_c: AmbContext => {
                    a_c.lhs.setExp(body)
                    a_c.rhs.setExp(body)
                    a_c
                }
                case p_c: ParserContext => {
                    p_c.exp = body
                    p_c
                }
            }
        }

        def setFold(folding: Boolean): ContextTree = {
            this match {
                case a_c: AmbContext => {
                    a_c.lhs.setFold(folding)
                    a_c.rhs.setFold(folding)
                    a_c
                }
                case p_c: ParserContext => {
                    p_c.folding = folding
                    p_c
                }
            }
        }

        def copy(): ContextTree
        def toString() : String
    }

    case class AmbContext(_lhs: ContextTree, _rhs: ContextTree, _id: Int) extends ContextTree{
        val lhs = _lhs
        val rhs = _rhs
        val id = _id
        def copy(): AmbContext = {
            new AmbContext(lhs.copy, rhs.copy, id)
        }
        override def toString: String = {
            val sb = new StringBuilder
            sb.append("{[lhs: " + lhs.toString + " ][rhs: " + rhs.toString + " ]}")
            sb.toString
        }
    }

    case class ParserContext(_pos: Int, start: PExp, _hash_table: HashMap[(Symbol, Int),Memo], startN: Symbol, _folding: Boolean) extends ContextTree{
        var pos = _pos
        var exp = start
        var hash_table = _hash_table
        var nonterm = startN
        var folding = _folding

        def copy(): ParserContext = {
            new ParserContext(pos, exp.copy, hash_table, nonterm, folding)
        }

        override def toString: String = {
            val sb = new StringBuilder
            sb.append("{pos<" + pos + ">exp<" + exp.toString + ">}")
            sb.toString
        }
    }

    class Memo(_context: ContextTree, _tree: List[Tree]){
        val context = _context
        val tree = _tree
        override def toString: String = {
            val sb = new StringBuilder
            sb.append("context: " + context + " tree: " + tree.toString)
            sb.toString
        }
    }

    def exec(start: Symbol, p: ParserContext): Option[(Tree, ContextTree)]={
        val (treeList, new_p) = parse(List.empty[Tree], p)
        println(lrbs)
        //println(new_p.hash_table)
        //return Some((Node(start, treeList), (input.drop(new_p.head.pos)).map(_.toChar).mkString))
        return Some((Node(start, disambiguity(treeList)), new_p))
    }

    def disambiguity(trees: List[Tree]): List[Tree] = {
        if(trees.isEmpty) return trees
        var new_trees = List.empty[Tree]
        for(tree <- trees){
            new_trees = new_trees:::disamb(tree)
        }
        new_trees
    }

    def disamb(tree: Tree): List[Tree] = {
        tree match {
            case Node(name, nexts) => List(Node(name, disambiguity(nexts)))
            case AmbNode(ambid, lhs, rhs) => {
                lrbs.get(ambid) match {
                    case Some(lrb) => lrb match {
                        case Left() => disambiguity(lhs)
                        case Right() => disambiguity(rhs)
                        case Both() => List()
                    }
                    case None => List(AmbNode(ambid, disambiguity(lhs), disambiguity(rhs)))
                }
            }
            case _ => List(tree) //throw new RuntimeException("error 1: can't do disambiguity")
        }
    }

    def amb_parse(tree: List[Tree], p: ContextTree):(List[Tree], ContextTree) = {
        p match {
            case p_c: ParserContext => {
                parse(tree, p_c)
            }
            case AmbContext(lhs, rhs, _id) => {
                val (lhs_tree, lhs_p) = amb_parse(List.empty[Tree], lhs)
                val (rhs_tree, rhs_p) = amb_parse(List.empty[Tree], rhs)
                lhs_p match {
                    case lhs_p_c: ParserContext => {
                        lhs_p_c.exp match {
                            case PFail(_) => rhs_p match {
                                case rhs_p_c : ParserContext => {
                                    rhs_p_c.exp match {
                                        case PFail(_) => {
                                            lrbs = lrbs + (_id -> Both())
                                            (disambiguity(tree), rhs_p_c) // want to return fail
                                        }
                                        case _ => {
                                            lrbs = lrbs + (_id -> Right())
                                            (disambiguity(tree):::rhs_tree, rhs_p_c)
                                        }
                                    }
                                }
                                case AmbContext(_, _, _) => {
                                    lrbs = lrbs + (_id -> Right())
                                    (disambiguity(tree):::rhs_tree, rhs_p)
                                }
                            }
                            case _ => rhs_p match {
                                case rhs_p_c : ParserContext => {
                                    rhs_p_c.exp match {
                                        case PFail(_) => {
                                            lrbs = lrbs + (_id -> Left())
                                            (disambiguity(tree):::lhs_tree, lhs_p_c)
                                        }
                                        case _ => (tree:+AmbNode(_id, lhs_tree, rhs_tree), AmbContext(lhs_p_c.copy, rhs_p_c.copy, _id))
                                    }
                                }
                                case AmbContext(_, _, _) => (tree:+AmbNode(_id , lhs_tree, rhs_tree), AmbContext(lhs_p_c.copy, rhs_p.copy, _id))
                            }
                        }
                    }
                    case AmbContext(_, _, _) => rhs_p match {
                        case rhs_p_c : ParserContext => {
                            rhs_p_c.exp match {
                                case PFail(_) => {
                                    lrbs = lrbs + (_id -> Left())
                                    (disambiguity(tree):::lhs_tree, lhs_p)
                                }
                                case _ => (tree:+AmbNode(_id, lhs_tree, rhs_tree), AmbContext(lhs_p.copy, rhs_p_c.copy, _id))
                            }
                        }
                        case AmbContext(_, _, _) => (tree:+AmbNode(_id, lhs_tree, rhs_tree), AmbContext(lhs_p.copy, rhs_p.copy, _id))
                    }
                }
            }
        }
    }

    def amb_memorized(tree: List[Tree], p: ContextTree):(List[Tree], ContextTree) = {
        p match {
            case p_c: ParserContext => {
                memorized(tree, p_c)
            }
            case AmbContext(lhs, rhs, _id) => {
                val (lhs_tree, lhs_p) = amb_memorized(List.empty[Tree], lhs)
                val (rhs_tree, rhs_p) = amb_memorized(List.empty[Tree], rhs)
                lhs_p match {
                    case lhs_p_c: ParserContext => {
                        lhs_p_c.exp match {
                            case PFail(_) => rhs_p match {
                                case rhs_p_c : ParserContext => {
                                    rhs_p_c.exp match {
                                        case PFail(_) => {
                                            lrbs = lrbs + (_id -> Both())
                                            (disambiguity(tree), rhs_p_c)
                                        }
                                        case _ => {
                                            lrbs = lrbs + (_id -> Right())
                                            (disambiguity(tree):::rhs_tree, rhs_p_c)
                                        }
                                    }
                                }
                                case AmbContext(_, _, _) => {
                                    lrbs = lrbs + (_id -> Right())
                                    (disambiguity(tree):::rhs_tree, rhs_p)
                                }
                            }
                            case _ => rhs_p match {
                                case rhs_p_c : ParserContext => {
                                    rhs_p_c.exp match {
                                        case PFail(_) => {
                                            lrbs = lrbs + (_id -> Left())
                                            (disambiguity(tree):::lhs_tree, lhs_p_c)
                                        }
                                        case _ => (tree:+AmbNode( _id, lhs_tree, rhs_tree), AmbContext(lhs_p_c.copy, rhs_p_c.copy, _id))
                                    }
                                }
                                case AmbContext(_, _, _) => (tree:+AmbNode( _id, lhs_tree, rhs_tree), AmbContext(lhs_p_c.copy, rhs_p.copy, _id))
                            }
                        }
                    }
                    case AmbContext(_, _, _) => rhs_p match {
                        case rhs_p_c : ParserContext => {
                            rhs_p_c.exp match {
                                case PFail(_) => {
                                    lrbs = lrbs + (_id -> Left())
                                    (disambiguity(tree):::lhs_tree, lhs_p)
                                }
                                case _ => (tree:+AmbNode(_id, lhs_tree, rhs_tree), AmbContext(lhs_p.copy, rhs_p_c.copy, _id))
                            }
                        }
                        case AmbContext(_, _, _) => (tree:+AmbNode(_id, lhs_tree, rhs_tree), AmbContext(lhs_p.copy, rhs_p.copy, _id))
                    }
                }
            }
        }
    }

    def parse(tree: List[Tree], p: ParserContext):(List[Tree], ContextTree) = {
               p.exp match {
                    case PSucc() => {
                        return (tree, p)
                    }
                    case PEmpty(next) => {
                        p.exp = next
                        return parse(tree, p)
                    }
                    case PFail(msg) => throw new RuntimeException(msg)
            
                    case PMatch(bytes, next) => {
                        val in = input
                        val bytes_length = bytes.length
                        if((bytes_length + p.pos) > in.length){
                            p.exp = PFail("")//String index out of range:" + (bytes.length + p.pos))
                            return (tree, p)
                        }
                        if(bytesEq(bytes, p.pos, bytes_length)){
                            p.exp = next
                            p.pos = p.pos + bytes_length
                            val new_tree = tree:+Leaf((bytes.map(_.toChar)).mkString)
                            parse(new_tree, p)
                        }else {
                            /**
                            if((bytes.map(_.toChar)).mkString == ""){ // case Empty
                                p.exp = next
                                //val new_tree = tree:+Leaf("")
                                return parse(tree, p)
                            }
                            */
                            p.exp = PFail("")//"pos: " + (p.pos + 1) + " string: "+ s + " -> don't match " + (bytes.map(_.toChar)).mkString)
                            (tree, p)
                        }
                    }
                    case PAny(next) => {
                        if((p.pos + 1) > input.length){
                            p.exp = PFail("String index out of range:" + (1 + p.pos))
                            return (tree, p)
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
                                
                                    val (child_tree, new_p) = amb_memorized(List.empty[Tree], p.copy)
                                    var new_tree = List.empty[Tree]
                                    //println(child_tree)
                                    new_p match {
                                        case p_c: ParserContext => {
                                            p_c.exp match {
                                                case PFail(_) => {
                                                    p.exp = p_c.exp
                                                    (tree, p)
                                                }
                                                case PEmpty(next) => {
                                                    p_c.exp = next 
                                                    parse(new_tree, p_c.copy)
                                                }
                                                case _ => {
                                                    p_c.exp = next
                                                    new_tree = tree:+Node(symbol,child_tree)
                                                    parse(new_tree, p_c.copy)
                                                }
                                            } 
                                        }
                                        case AmbContext(_, _, _) => {
                                            new_tree = tree:+Node(symbol,child_tree)
                                            next match {
                                                case PSucc() => (new_tree, new_p)
                                                case _ => {
                                                    amb_parse(new_tree, new_p.setExp(next))
                                                }
                                            }
                                        }
                                    }
                            }
                        
                            case None => throw new RuntimeException(symbol + ": Rule can not be found")
                        }
                    }
                
            
                    case PIf(lhs, rhs, next) => {
                        p.exp = lhs
                        val (lhs_tree, lhs_p) = parse(tree, p.copy)
                        lhs_p match {
                            case lhs_p_c: ParserContext => {
                                lhs_p_c.exp match {
                                    case PFail(_) => {
                                        p.exp = rhs
                                        val (rhs_tree, rhs_p) = parse(tree,p.copy)
                                        rhs_p match {
                                            case rhs_p_c: ParserContext => {
                                                rhs_p_c.exp match {
                                                    case PFail(msg) => {
                                                        p.exp = PFail(msg)
                                                        (tree, p)
                                                    }
                                                    case _ => {
                                                        rhs_p_c.exp = next
                                                        parse(rhs_tree, rhs_p_c.copy) 
                                                    }
                                                }
                                            }
                                            case AmbContext(_, _, _) => {
                                                rhs_p.setExp(next)
                                                amb_parse(rhs_tree, rhs_p) 
                                            }
                                        }
                                    }
                                    case _ => {
                                        lhs_p_c.exp = next
                                        parse(lhs_tree, lhs_p_c.copy)
                                    }
                                }
                            }
                            case AmbContext(_, _, _) => {
                                lhs_p.setExp(next)
                                amb_parse(lhs_tree, lhs_p)
                            }
                        }
                    }

                    case PUnion(lhs, rhs) => {   
                        //println("tree: " + tree )
                        //println("p: " + p )      
                        p.exp = lhs
                        val (lhs_tree, lhs_p) = parse(tree, p.copy)
                        lhs_p match {
                            case lhs_p_c: ParserContext => {
                                lhs_p_c.exp match {
                                    case PFail(_) => {
                                        p.exp = rhs
                                        val (rhs_tree, rhs_p) = parse(tree,p.copy)
                                        rhs_p match {
                                            case rhs_p_c: ParserContext => {
                                                rhs_p_c.exp match {
                                                    case PFail(msg) => {                                                              
                                                        p.exp = PFail(msg)
                                                        (tree, p)
                                                    }
                                                    case _ => {
                                                        (rhs_tree, rhs_p_c.copy)
                                                    }
                                                }
                                            }
                                            case AmbContext(_, _, _) => {
                                                (rhs_tree, rhs_p)
                                            }
                                        }
                                    }
                                    case _ => {
                                        p.exp = rhs
                                        val (rhs_tree, rhs_p) = parse(tree,p.copy)
                                        rhs_p match {
                                            case rhs_p_c: ParserContext => {
                                                rhs_p_c.exp match {
                                                    case PFail(_) => {
                                                        (lhs_tree, lhs_p_c.copy)
                                                    }
                                                    case _ => {
                                                        if(isEqualPos(lhs_p_c.copy, rhs_p_c.copy)){
                                                            val new_tree = tree:+AmbNode(id, lhs_tree, rhs_tree)
                                                            id += 1
                                                            (new_tree, lhs_p_c.copy)
                                                        }else {
                                                            val new_tree = tree:+AmbNode(id, lhs_tree, rhs_tree)
                                                            val new_p = AmbContext(lhs_p_c.copy, rhs_p_c.copy, id)
                                                            id += 1
                                                            (new_tree, new_p)
                                                        }
                                                    }
                                                }
                                            }
                                            case AmbContext(_, _, _) => {
                                                if(isEqualPos(lhs_p_c.copy,rhs_p.copy)){
                                                    val new_tree = tree:+AmbNode(id, lhs_tree, rhs_tree)
                                                    id += 1
                                                    (new_tree, lhs_p_c.copy)
                                                }else {
                                                    val new_tree = tree:+AmbNode(id, lhs_tree, rhs_tree)
                                                    val new_p = AmbContext(lhs_p_c.copy, rhs_p.copy, id)
                                                    id += 1
                                                    (new_tree, new_p)
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            case AmbContext(_, _, _) => {
                                p.exp = rhs
                                val (rhs_tree, rhs_p) = parse(tree,p.copy)
                                rhs_p match {
                                    case rhs_p_c: ParserContext => {
                                        rhs_p_c.exp match {
                                            case PFail(_) => {
                                                (lhs_tree, lhs_p)
                                            }
                                            case _ => {
                                                if(isEqualPos(lhs_p.copy,rhs_p.copy)){
                                                    val new_tree = tree:+AmbNode(id, lhs_tree, rhs_tree)
                                                    id += 1
                                                    (new_tree, lhs_p)
                                                }else {
                                                    val new_tree = tree:+AmbNode(id, lhs_tree, rhs_tree)
                                                    val new_p = AmbContext(lhs_p.copy, rhs_p_c.copy, id)
                                                    id += 1
                                                    (new_tree, new_p)
                                                }
                                            }
                                        }
                                    }
                                    case AmbContext(_, _, _) => {
                                        if(isEqualPos(lhs_p.copy,rhs_p.copy)){
                                            val new_tree = tree:+AmbNode(id, lhs_tree, rhs_tree)
                                            id += 1
                                            (new_tree, lhs_p)
                                        }else {
                                            val new_tree = tree:+AmbNode(id, lhs_tree, rhs_tree)
                                            val new_p = AmbContext(lhs_p.copy, rhs_p.copy, id)
                                            id += 1
                                            (new_tree, new_p)
                                        }
                                    }
                                }
                            }
                        }
                    }
            
                    case PNot(body, next) => {
                        p.exp = body
                        val (new_tree, new_p) = parse(tree, p.copy)
                        new_p match {
                            case p_c: ParserContext => {
                                p_c.exp match {
                                    case PFail(_) => {
                                        p_c.exp = next
                                        parse(tree, p_c)
                                    }
                                    case _ => {
                                        p_c.exp = PFail("Match PExp: " + body)
                                        parse(tree, p_c)
                                    }
                                }
                            }
                            case AmbContext(lhs, rhs, _) => {
                                lhs.setExp(PFail("Match PExp: " + body))
                                rhs.setExp(PFail("Match PExp: " + body))
                                amb_parse(tree, new_p)
                            }
                        }
                    }

                    case PAnd(body, next) => {
                        p.exp = body
                        val (new_tree, new_p) = parse(tree, p.copy)
                        new_p match {
                            case p_c: ParserContext => {
                                p_c.exp match {
                                    case PFail(_) => {
                                        p_c.exp = PFail("Dont't match PExp: " + body)
                                        parse(tree, p_c)
                                    }
                                    case _ => {
                                        p_c.exp = next
                                        parse(tree, p_c)
                                    }
                                }
                            }
                            case AmbContext(lhs, rhs, _) => {
                                lhs.setExp(next)
                                rhs.setExp(next)
                                amb_parse(tree, new_p)
                            }
                        }
                    }

                    case PMany(body, next) => {
                        p.exp = body
                        val (new_tree, new_p) = many(tree, p)
                        amb_parse(new_tree, new_p.setExp(next))
                    }

                    case PFold(name, body, rec, next) => {                        
                        println(tree)
                        if(p.folding){
                            p.exp = PFold(name, body, rec, next)
                            (tree, p)
                        }else{
                            val (new_tree, new_p) = fold(name, body, rec, tree, p.setFold(true))
                            amb_parse(new_tree, new_p.setExp(next).setFold(false))
                        }
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

    def many(tree: List[Tree], p: ParserContext): (List[Tree], ContextTree) = {
        var result = true
        val body = p.exp
        var (now_tree, now_p: ContextTree) = (tree,p)

        while(result){
            val (new_tree: List[Tree], new_p: ContextTree) = amb_parse(now_tree,now_p.copy)
            new_p match {
                    case p_c: ParserContext => {
                        p_c.exp match {
                            case PFail(_) => {
                                result = false
                            }
                            case _ => {
                                p_c.exp = body
                                now_tree = new_tree
                                now_p = new_p
                            }
                        }
                    }
                    case AmbContext(lhs, rhs, _) => {
                        lhs.setExp(body)
                        rhs.setExp(body)
                        now_tree = new_tree
                        now_p = new_p
                    }
                }
        }
        
        (now_tree, now_p)
    }
/**
    def fold(name: Symbol, body: PExp, tree: List[Tree], p: ContextTree, folding: Boolean): (List[Tree], ContextTree) = {
        
        if(folding) {
            println("test")       
            val (new_tree, new_p) = amb_parse(List.empty[Tree],p.setExp(body).setFold(true).copy)
            new_p match {
                case p_c: ParserContext => {
                    p_c.exp match {
                        case PFail(_) => {
                            println("test")
                            (tree, p)
                        }
                        case _ => {
                            println("test")
                            (tree, p.setExp(body))
                        }
                    
                    }
                }
                case AmbContext(_, _, _) => {
                     (tree, p.setExp(body))
                }
            }
        }else {
            val (new_tree, new_p) = amb_parse(tree,p.setExp(body).setFold(true).copy)
            println(new_tree)
            println(new_p)
            new_p match {
                case p_c: ParserContext => {
                    p_c.exp match {
                        case PFail(_) => {
                            p_c.folding = false
                            (tree, p_c)
                        }
                        case _ => {
                            
                            rules.get(name) match {
                                case Some(exp) => {
                                    p_c.exp = exp
                                    p_c.folding = false
                                    println(p_c)
                                    fold(name, body, tree:::new_tree, p_c, false)
                                }
                                case None => throw new RuntimeException(name + ": Rule can not be found")
                            }
                            
                            p_c.folding = false
                            println(p_c)
                            fold(name, body, new_tree, p_c, false)
                        }
                    
                    }
                }
                case AmbContext(_, _, _) => {
                     amb_parse(tree:::new_tree, new_p)
                }
            }
        }
    }
*/
    def fold(name: Symbol, body: PExp, rec: Symbol, tree: List[Tree], p: ContextTree): (List[Tree], ContextTree) = {
        val (body_tree, body_p) = amb_parse(tree, p.setExp(body))
        println(body_p)
        body_p match {
                case body_p_c: ParserContext => {
                    body_p_c.exp match {
                        case PFail(_) => {
                            (tree, p)
                        }
                        case _ => {
                            rules.get(rec) match {
                                case Some(exp) => {
                                    body_p_c.exp = exp
                                    val (rec_tree, rec_p) = parse(body_tree, body_p_c)
                                    rec_p match {
                                        case rec_p_c: ParserContext => {
                                            rec_p_c.exp match {
                                                case PFail(_) => {
                                                    (tree, p)
                                                }
                                                case PSucc() => {
                                                    fold(name, body, rec, List(Node(name, rec_tree)), rec_p_c)
                                                }
                                                case _ => amb_parse(List(Node(name, rec_tree)), rec_p.setFold(false))
                                            }
                                        }
                                        case AmbContext(_, _, _) => {
                                            amb_parse(List(Node(name, rec_tree)), rec_p.setFold(false))
                                        }
                                    }
                                }
                                case None => throw new RuntimeException(rec + ": Rule can not be found")
                            }
                        }
                    
                    }
                }
                case AmbContext(_, _, _) => {
                    rules.get(rec) match {
                        case Some(exp) => {
                            val (rec_tree, rec_p) = amb_parse(body_tree, body_p.setExp(exp))
                            amb_parse(List(Node(name, rec_tree)), rec_p.setFold(false))
                        }
                        case None => throw new RuntimeException(rec + ": Rule can not be found")
                    }
                }
        }
    }

    def isEqualPos(lhs: ContextTree, rhs: ContextTree): Boolean = {
        var pos: Int = 0
        lhs match {
            case ParserContext(_pos, _, _, _, _) => {
                pos = _pos
            }
            case AmbContext(lhs, rhs, _) => {
                return isEqualPos(lhs, rhs)
            }
        }
        rhs match {
            case ParserContext(_pos, _, _, _, _) => {
                return pos == _pos
            }
            case AmbContext(lhs, rhs, _) => {
                return isEqualPos(lhs, rhs)
            }
        }
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

    def memorized(tree: List[Tree], p: ParserContext):(List[Tree], ContextTree) = {
        p.hash_table.get((p.nonterm,p.pos)) match {
            case Some(memo) => {
                //println("memo: " + memo.tree)
                //println(p.pos + " " + p.nonterm)
                //println(memo.pos + " " + memo.nonterm + " " + memo.exp)
                //p.pos = memo.pos
                //p.nonterm = memo.nonterm
                //p.exp = memo.exp
                (memo.tree, memo.context)
            }
            case None => {
                val memo_info = (p.nonterm, p.pos)
                val (new_tree, new_p) = parse(tree, p)
                new_p match {
                    case ParserContext(_, exp, _, _, _) => {
                        exp match {
                            case PFail(_) => {
                                //p.hash_table += ((memo_info) -> new Memo(new_p.pos, new_p.exp, new_p.nonterm, new_tree))
                            }
                            case _ => {
                                p.hash_table += ((memo_info) -> new Memo(new_p, new_tree))
                            }
                        }
                    }
                    case AmbContext(_, _, _) => {
                        //p.hash_table += ((memo_info) -> new Memo(new_p, new_tree))
                    }
                }
                
                //p.hash_table += ((memo_info) -> new Memo(new_p.pos, new_p.exp, new_p.nonterm, new_tree))
                //println(p.hash_table)
                (new_tree, new_p)
            }
        }
    }

}