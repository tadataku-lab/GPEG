import AST._
import Forest._
import ParserContext._
import Memo._

object PackratParser{  
    def peg_parse(g: PGrammar, input: String): Option[(Forest, ParserContext)] = {
        val new_p = new PackratParser().packrat_parse(new ParserContext(PCall(g.start, PSucc()) , g.rules, input.getBytes, g.start)).save_top
        return Some((new_p.forest, new_p))
    }

    class PackratParser(){
        def packrat_parse(p: ParserContext): ParserContext = {
            parse(p)
        }

        def map_match(p: ParserContext, bytes: Array[Byte]): List[State] = {
            p.states = p.states.flatMap(state => p.match_bytes(state, bytes))
            p.states
        }

        def map_call(p: ParserContext, symbol: Symbol): List[State] = {
            p.states = p.states.flatMap(state => call_symbol(p, state, symbol))
            p.states
        }

        def call_symbol(p: ParserContext, state: State, symbol: Symbol): List[State] = {
            parse(p.set_exp(p.lookUpRules(symbol)).set_states(List(state.new_state))).save_node(symbol, state).states
        }

        def map_union(p: ParserContext, lhs: PExp, rhs: PExp): List[State] = {
            p.states = p.states.flatMap(state => parse_union(p, lhs, rhs, state))
            p.states
        }

        def parse_union(p: ParserContext, lhs: PExp, rhs: PExp, state: State): List[State] = {
            val lhs_result = parse(p.set_exp(lhs).set_states(List(state.new_state))).get_result
            val rhs_result = parse(p.set_exp(rhs).set_states(List(state.new_state))).get_result
            (lhs_result._2, rhs_result._2) match {
                case (PFail(_), PFail(_)) => List()
                case (_, PFail(_)) => p.add_either(lhs_result._1, state)
                case (PFail(_), _) => p.add_either(rhs_result._1, state)
                case (_, _) =>  p.add_both(lhs_result._1, rhs_result._1, state)
            }
        }

        def parse(p: ParserContext): ParserContext = {
            p.exp match {
                case PSucc() => {
                    return (p)
                }
                case PEmpty(next) => {
                    p.exp = next
                    return parse(p)
                }
                case PFail(msg) => throw new RuntimeException(msg)

                case PMatch(bytes, next) => if(map_match(p, bytes).isEmpty) p.set_exp(PFail("")) else parse(p.set_exp(next))
        
                /**
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
                */

                case PCall(symbol, next) => if(map_call(p, symbol).isEmpty) p.set_exp(PFail("")) else parse(p.set_exp(next))

                /**
                case PCall(symbol, next) => {
                    p.table.get((symbol, p.pos)) match {
                        case Some(memo) => {
                            memo.v match {
                                case Some(succ) => {
                                    val (next_tree, next_p) = new_id(List(Node(symbol,succ._1)), succ._2.copy)
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
                */
            
                /**
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
                */
                
                case PUnion(lhs, rhs) => if(map_union(p, lhs, rhs).isEmpty) p.set_exp(PFail("")) else p
                
                /**
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
                */

                /**
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
                */
            }
        }        
    }
}