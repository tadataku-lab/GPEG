import AST._
import Tree._
import ContextTree._
import Memo._
import scala.collection.mutable.{HashMap}

object PackratParser{

    sealed trait LorRorB

    case class Left() extends LorRorB
    case class Right() extends LorRorB
    case class Both() extends LorRorB

    def peg_parse(g: PGrammar, input: String): Option[(Tree, ContextTree)] = {
        val start = 
        g.rules.get(g.start) match {
            case Some(exp) => exp
            case None => throw new RuntimeException(g.start + ": Rule can not be found")
        }
        val (treeList, new_p) = new PackratParser(g.rules, input.getBytes).disamb_parse(List.empty[Tree], new ParserContext(0, start, g.start, false))
        return Some((Node(g.start, treeList), new_p))
    }

    class PackratParser(rules: Map[Symbol, PExp], input: Array[Byte]){
        private val RULES: Map[Symbol, PExp] = rules
        private val INPUT: Array[Byte] = input
        private var ID: Int = 0
        private var MAPID: Map[Int, Int] = Map.empty[Int, Int]
        private var LRBs: Map[Int, LorRorB] = Map.empty[Int,LorRorB]
        var HASHTABLE: HashMap[(Symbol, Int), Memo] = new HashMap[(Symbol, Int),Memo]
        var BENCH: Array[Long] = Array(0,0,0,0)
        var BENCHTIME: Array[Long] = Array(0,0,0,0)

        def disamb_context(context: ContextTree): ContextTree = {
            val start = System.currentTimeMillis
            val _context = bench_disamb_context(context)
            val time = System.currentTimeMillis - start
            BENCHTIME(0) += time
            _context
        }

        def bench_disamb_context(context: ContextTree): ContextTree = {
            BENCH(0) += 1
            context match{
                case p_c: ParserContext => p_c
                case AmbContext(lhs, rhs, ambid) => {
                    LRBs.get(ambid) match {
                        case Some(lrb) => lrb match {
                            case Left() => bench_disamb_context(lhs)
                            case Right() => bench_disamb_context(rhs)
                            case Both() => null
                        }
                        case None => AmbContext(bench_disamb_context(lhs), bench_disamb_context(rhs), ambid)
                    }
                }
                case null => null
            }
        }

        def disambiguity(trees: List[Tree]): List[Tree] = {
            val start = System.currentTimeMillis
            val new_trees = bench_disambiguity(trees)
            val time = System.currentTimeMillis - start
            BENCHTIME(1) += time
            new_trees
            
        }

        def bench_disambiguity(trees: List[Tree]): List[Tree] = {
            if(trees.isEmpty) return trees
            var new_trees = List.empty[Tree]
            for(tree <- trees){
                new_trees = new_trees:::disamb(tree)
            }
            new_trees
        }

        def disamb(tree: Tree): List[Tree] = {
            BENCH(1) += 1
            tree match {
                case Node(name, nexts) => List(Node(name, bench_disambiguity(nexts)))
                case AmbNode(ambid, lhs, rhs) => {
                    LRBs.get(ambid) match {
                        case Some(lrb) => lrb match {
                            case Left() => bench_disambiguity(lhs)
                            case Right() => bench_disambiguity(rhs)
                            case Both() => List()
                        }
                        case None => List(AmbNode(ambid, bench_disambiguity(lhs), bench_disambiguity(rhs)))
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
                    amb_match(tree, _id, lhs_tree, lhs_p, rhs_tree, rhs_p)
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
                    amb_match(tree, _id, lhs_tree, lhs_p, rhs_tree, rhs_p)
                }
            }
        }

        def amb_match(tree: List[Tree], _id: Int,  lhs_tree: List[Tree], lhs_p: ContextTree, rhs_tree: List[Tree], rhs_p: ContextTree):(List[Tree], ContextTree) = {
            lhs_p match {
                case lhs_p_c: ParserContext => {
                    lhs_p_c.exp match {
                        case PFail(_) => rhs_p match {
                            case rhs_p_c : ParserContext => {
                                rhs_p_c.exp match {
                                    case PFail(_) => {
                                        LRBs += (_id -> Both())
                                        (disambiguity(tree), disamb_context(rhs_p_c))
                                    }
                                    case _ => {
                                        LRBs += (_id -> Right())
                                        (disambiguity(tree):::rhs_tree, disamb_context(rhs_p_c))
                                    }
                                }
                            }
                            case AmbContext(_, _, _) => {
                                LRBs += (_id -> Right())
                                (disambiguity(tree):::rhs_tree, disamb_context(rhs_p))
                            }
                        }
                        case _ => rhs_p match {
                            case rhs_p_c : ParserContext => {
                                rhs_p_c.exp match {
                                    case PFail(_) => {
                                        LRBs += (_id -> Left())
                                        (disambiguity(tree):::lhs_tree, disamb_context(lhs_p_c))
                                    }
                                    case _ => (tree:+AmbNode( _id, lhs_tree, rhs_tree), disamb_context(AmbContext(lhs_p_c.copy, rhs_p_c.copy, _id)))
                                }
                            }
                            case AmbContext(_, _, _) => (tree:+AmbNode( _id, lhs_tree, rhs_tree), disamb_context(AmbContext(lhs_p_c.copy, rhs_p.copy, _id)))
                        }
                    }
                }
                case AmbContext(_, _, _) => rhs_p match {
                    case rhs_p_c : ParserContext => {
                        rhs_p_c.exp match {
                            case PFail(_) => {
                                LRBs += (_id -> Left())
                                (disambiguity(tree):::lhs_tree, disamb_context(lhs_p))
                            }
                            case _ => (tree:+AmbNode(_id, lhs_tree, rhs_tree), disamb_context(AmbContext(lhs_p.copy, rhs_p_c.copy, _id)))
                        }
                    }
                    case AmbContext(_, _, _) => (tree:+AmbNode(_id, lhs_tree, rhs_tree), disamb_context(AmbContext(lhs_p.copy, rhs_p.copy, _id)))
                }
            }
        }

        def disamb_parse(tree: List[Tree], p: ParserContext):(List[Tree], ContextTree) = {
            val (new_tree, new_p) = parse(tree, p)
            val result = (disambiguity(new_tree), new_p)
            //println(HASHTABLE)
            //println(LRBs)
            println("disamb_context: " + BENCH(0) + "回 " + BENCHTIME(0) + "[ms]")
            println("disamb: " + BENCH(1) + "回 " + BENCHTIME(1) + "[ms]")
            println("renew_id_p: " + BENCH(2) + "回 " )
            println("renew_id_tree: " + BENCH(3) + "回 ")
            println("renew_id: " + BENCHTIME(2) + "[ms]")
            result
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
                            val in = INPUT
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
                            HASHTABLE.get((symbol, p.pos)) match {
                                case Some(memo) => {
                                    memo.v match {
                                        case Some(succ) => {
                                            val (next_tree, next_p) = renew_id(List(Node(symbol,succ._1)), succ._2.copy)
                                            next match {
                                                case PSucc() => (next_tree, next_p.setExp(next))
                                                case _ => amb_parse(next_tree, next_p.setExp(next))
                                            }
                                        }
                                        case None => {
                                            p.exp = PFail("")
                                            (tree, p)
                                        }
                                    }
                                }
                                case None => {
                                    RULES.get(symbol) match {
                                        case Some(exp) => {
                                            p.exp = exp
                                            p.nonterm = symbol
                                            val memo_info = (symbol, p.pos)
                                                val (child_tree, new_p) = parse(List.empty[Tree], p.copy)
                                                var new_tree = List.empty[Tree]
                                                //println(child_tree + " context: " + new_p)
                                                new_p match {
                                                    case p_c: ParserContext => {
                                                        p_c.exp match {
                                                            case PFail(_) => {
                                                                p.exp = p_c.exp
                                                                HASHTABLE += ((memo_info) -> new Memo(None))
                                                                (tree, p)
                                                            }
                                                            case PEmpty(next) => {
                                                                p_c.exp = next
                                                                //HASHTABLE += ((memo_info) -> new Memo(Some((child_tree, new_p))))
                                                                parse(new_tree, p_c.copy)
                                                            }
                                                            /**
                                                            case PSucc() => {
                                                                new_tree = tree:+Node(symbol,child_tree)
                                                                HASHTABLE += ((memo_info) -> new Memo(Some((new_tree, new_p))))
                                                                (new_tree, new_p)
                                                            }
                                                            */
                                                            case _ => {
                                                                HASHTABLE += ((memo_info) -> new Memo(Some((child_tree, p_c.copy))))
                                                                p_c.exp = next
                                                                new_tree = tree:+Node(symbol,child_tree)
                                                                //println("new: "+ new_tree + " next: " + p_c)
                                                                next match {
                                                                    case PSucc() => (new_tree, p_c)
                                                                    case _ => parse(new_tree, p_c)
                                                                }
                                                            }
                                                        } 
                                                    }
                                                    case AmbContext(_, _, _) => {
                                                        HASHTABLE += ((memo_info) -> new Memo(Some((child_tree, new_p.copy))))
                                                        new_tree = tree:+Node(symbol,child_tree)
                                                        //println("new: "+ new_tree + " next: " + next)
                                                        next match {
                                                            case PSucc() => {
                                                                (new_tree, new_p)
                                                            }
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
                                                                val new_tree = tree:+AmbNode(ID, lhs_tree, rhs_tree)
                                                                ID += 1
                                                                (new_tree, lhs_p_c.copy)
                                                            }else {
                                                                val new_tree = tree:+AmbNode(ID, lhs_tree, rhs_tree)
                                                                val new_p = AmbContext(lhs_p_c.copy, rhs_p_c.copy, ID)
                                                                ID += 1
                                                                (new_tree, new_p)
                                                            }
                                                        }
                                                    }
                                                }
                                                case AmbContext(_, _, _) => {
                                                    if(isEqualPos(lhs_p_c.copy,rhs_p.copy)){
                                                        val new_tree = tree:+AmbNode(ID, lhs_tree, rhs_tree)
                                                        ID += 1
                                                        (new_tree, lhs_p_c.copy)
                                                    }else {
                                                        val new_tree = tree:+AmbNode(ID, lhs_tree, rhs_tree)
                                                        val new_p = AmbContext(lhs_p_c.copy, rhs_p.copy, ID)
                                                        ID += 1
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
                                                        val new_tree = tree:+AmbNode(ID, lhs_tree, rhs_tree)
                                                        ID += 1
                                                        (new_tree, lhs_p)
                                                    }else {
                                                        val new_tree = tree:+AmbNode(ID, lhs_tree, rhs_tree)
                                                        val new_p = AmbContext(lhs_p.copy, rhs_p_c.copy, ID)
                                                        ID += 1
                                                        (new_tree, new_p)
                                                    }
                                                }
                                            }
                                        }
                                        case AmbContext(_, _, _) => {
                                            if(isEqualPos(lhs_p.copy,rhs_p.copy)){
                                                val new_tree = tree:+AmbNode(ID, lhs_tree, rhs_tree)
                                                ID += 1
                                                (new_tree, lhs_p)
                                            }else {
                                                val new_tree = tree:+AmbNode(ID, lhs_tree, rhs_tree)
                                                val new_p = AmbContext(lhs_p.copy, rhs_p.copy, ID)
                                                ID += 1
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

        def fold(name: Symbol, body: PExp, rec: Symbol, tree: List[Tree], p: ContextTree): (List[Tree], ContextTree) = {
            val (body_tree, body_p) = amb_parse(tree, p.setExp(body))
            rules.get(rec) match {
                case Some(exp) => body_parse(name, body, rec, body_tree, body_p, tree, p, exp)
                case None => throw new RuntimeException(rec + ": Rule can not be found")
            }
        }

        def body_parse(name: Symbol, body: PExp, rec: Symbol, body_tree: List[Tree], body_p: ContextTree, fail_tree: List[Tree], fail_p: ContextTree, exp: PExp): (List[Tree], ContextTree) = {
            body_p match {
                case body_p_c: ParserContext => {
                    body_p_c.exp match {
                        case PFail(_) => {
                            (fail_tree, fail_p)
                        }
                        case _ => {
                            body_p_c.exp = exp
                            val (rec_tree, rec_p) = amb_parse(List.empty[Tree], body_p)
                            rec_parse(name, body, rec, body_tree, rec_tree, rec_p, fail_tree, fail_p)
                        }
                    
                    }
                }
                case AmbContext(lhs, rhs, _id) => {
                    val (lhs_tree, lhs_p) = body_parse(name, body, rec, List.empty[Tree], lhs, fail_tree, fail_p, exp)
                    val (rhs_tree, rhs_p) = body_parse(name, body, rec, List.empty[Tree], rhs, fail_tree, fail_p, exp)
                    amb_match(body_tree, _id, lhs_tree, lhs_p, rhs_tree, rhs_p)
                }
            }
        }

        def fold_parse(name: Symbol, body: PExp, rec: Symbol, rec_tree: List[Tree], rec_p: ContextTree, fail_tree: List[Tree], fail_p: ContextTree): (List[Tree], ContextTree) = {
            val (fold_tree, fold_p) = fold(name, body, rec, rec_tree, rec_p)
            fold_p match {
                case fold_p_c: ParserContext => {
                    fold_p_c.exp match {
                        case PFail(_) => (fail_tree, fail_p)
                        case _ => (fold_tree, fold_p)
                    }
                }
                case fold_a_c: AmbContext => {
                    (fold_tree, fold_p)
                }
            }
        }

        def rec_parse(name: Symbol, body: PExp, rec: Symbol, body_tree: List[Tree], rec_tree: List[Tree], rec_p: ContextTree, fail_tree: List[Tree], fail_p: ContextTree): (List[Tree], ContextTree) = {
            rec_p match {
                case rec_p_c: ParserContext => {
                    rec_p_c.exp match {
                        case PFail(_) => {
                            (fail_tree, fail_p)
                        }
                        case PSucc() => {
                            fold_parse(name, body, rec, List(Node(name, body_tree:+Node(rec, rec_tree))), rec_p_c.copy, body_tree:+Node(rec, rec_tree), rec_p_c)
                        }
                        case _ => {
                            val (new_tree, new_p) = amb_parse(List(Node(name, body_tree:+Node(rec, rec_tree))), rec_p.setFold(false))
                            fold_parse(name, body, rec, new_tree, new_p.copy, new_tree, new_p)
                        }
                    }
                }
                case AmbContext(lhs, rhs, _id) => {
                    val (lhs_tree, lhs_p) = rec_parse(name, body, rec, rec_tree, List.empty[Tree], lhs, fail_tree, fail_p)
                    val (rhs_tree, rhs_p) = rec_parse(name, body, rec, rec_tree, List.empty[Tree], rhs, fail_tree, fail_p)
                    amb_match(body_tree, _id, lhs_tree, lhs_p, rhs_tree, rhs_p)
                }
            }
        }

        def isEqualPos(lhs: ContextTree, rhs: ContextTree): Boolean = {
            var pos: Int = 0
            lhs match {
                case ParserContext(_pos, _, _, _) => {
                    pos = _pos
                }
                case AmbContext(lhs, rhs, _) => {
                    return isEqualPos(lhs, rhs)
                }
            }
            rhs match {
                case ParserContext(_pos, _, _, _) => {
                    return pos == _pos
                }
                case AmbContext(lhs, rhs, _) => {
                    return isEqualPos(lhs, rhs)
                }
            }
        }

        def bytesEq(bytes: Array[Byte], pos: Int, length: Int):Boolean = {
            if(bytes.length == length) bytes.sameElements(input.slice(pos, pos + length)) else throw new Exception("don't match length")
        }

        def memorized(tree: List[Tree], p: ParserContext):(List[Tree], ContextTree) = {
            HASHTABLE.get((p.nonterm,p.pos)) match {
                case Some(memo) => {
                    memo.v match {
                        case Some(succ) => renew_id(succ._1, succ._2)
                        case None => {
                            p.exp = PFail("")
                            (tree, p)
                        }
                    }
                }
                case None => {
                    val memo_info = (p.nonterm, p.pos)
                    val (new_tree, new_p) = parse(tree, p)
                    new_p match {
                        case ParserContext(_, exp, _, _) => {
                            exp match {
                                case PFail(_) => {
                                    HASHTABLE += ((memo_info) -> new Memo(None))
                                }
                                case _ => {
                                    HASHTABLE += ((memo_info) -> new Memo(Some((new_tree, new_p))))
                                }
                            }
                        }
                        case AmbContext(_, _, _) => {
                            HASHTABLE += ((memo_info) -> new Memo(Some((new_tree, new_p))))
                        }
                    }
                    
                    //p.hash_table += ((memo_info) -> new Memo(new_p.pos, new_p.exp, new_p.nonterm, new_tree))
                    (new_tree, new_p)
                }
            }
        }

        def renew_id(trees: List[Tree], p: ContextTree): (List[Tree], ContextTree) = {
            val start = System.currentTimeMillis
            val renew_p = renew_id_p(p)
            val renew_trees = renew_id_trees(trees)
            val time = System.currentTimeMillis - start
            BENCHTIME(2) += time
            (renew_trees, renew_p)
        }

        def renew_id_p(p: ContextTree): ContextTree = {
            BENCH(2) += 1
            p match {
                case p_c: ParserContext => p_c
                case AmbContext(lhs, rhs, _id) => {
                    val new_id = ID;
                    MAPID += (_id -> new_id)
                    ID += 1
                    AmbContext(renew_id_p(lhs), renew_id_p(rhs), new_id)
                }
            }
        }

        def renew_id_trees(trees: List[Tree]): List[Tree] = {
            val _trees = trees.map(tree => renew_id_tree(tree))
            _trees
        }

        def renew_id_tree(tree: Tree): Tree = {
            BENCH(3) += 1
            tree match {
                case leaf: Leaf => leaf
                case Node(name, next) => Node(name, renew_id_trees(next))
                case AmbNode(_id, lhs, rhs) => {
                    MAPID.get(_id) match {
                        case Some(new_id) => {
                            AmbNode(new_id, renew_id_trees(lhs), renew_id_trees(rhs))
                        }
                        case None => {
                            val new_id = ID
                            MAPID += (_id -> new_id)
                            ID += 1
                            AmbNode(new_id, renew_id_trees(lhs), renew_id_trees(rhs))
                        }
                    }
                }
            }
        }
    }
}