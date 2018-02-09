import AST._
import Tree._
import ParserContext._
import scala.collection.mutable.{ArrayBuffer}

object PackratParser{  
    def peg_parse(g: PGrammar, input: String): Option[(Tree, ParserContext)] = {
        val new_p = new PackratParser().packrat_parse(new ParserContext(g.rules(g.start) , g.rules, input.getBytes))
        return Some((new_p.merge.newAmbNode(g.start), new_p))
    }

    class PackratParser(){
        def packrat_parse(p: ParserContext): ParserContext = {
            parse(p)
        }

        def map_match(p: ParserContext, bytes: Array[Byte]): ArrayBuffer[State] = {
            p.states = p.states.flatMap(state => p.match_bytes(state, bytes))
            p.states
        }

        def map_call(p: ParserContext, symbol: Symbol): ArrayBuffer[State] = {
            val s = p.states
            p.states = s.flatMap(state => lookup(p, symbol, state))
            p.states
        }

        def lookup(p: ParserContext, symbol: Symbol, state: State): ArrayBuffer[State] = {
            p.lookup(symbol, state.pos) match{
                case Some(states) => states.map(s => s.copy().update(symbol, state))
                case None => call_symbol(p, symbol, state)
            }
        }

        def call_symbol(p: ParserContext, symbol: Symbol, state: State): ArrayBuffer[State] = {
            parse(p.set_exp(p.rules(symbol)).set_states(ArrayBuffer(state.newState))).merge.memo(symbol, state.pos).map(s => s.update(symbol, state))
        }

        def map_union(p: ParserContext, lhs: PExp, rhs: PExp): ArrayBuffer[State] = {
            p.states = p.states.flatMap(state => union(p, lhs, rhs, state))
            p.states
        }

        def union(p: ParserContext, lhs: PExp, rhs: PExp, state: State): ArrayBuffer[State] = {
            parse(p.set_exp(lhs).set_states(ArrayBuffer(state.copy))).states++parse(p.set_exp(rhs).set_states(ArrayBuffer(state.copy))).states
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