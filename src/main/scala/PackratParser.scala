import AST._
import Tree._
import ParserContext._
import scala.collection.mutable.{BitSet}

object PackratParser{  
    def peg_parse(g: PGrammar, input: String): Option[(Tree, ParserContext)] = {
        val new_p = new PackratParser().packrat_parse(new ParserContext(g.rules(g.start) , g.rules, g.symbols, input.getBytes))
        return Some((new_p.makeAmbNode(g.symbols(g.start)), new_p))
    }

    class PackratParser(){

        def packrat_parse(p: ParserContext): ParserContext = parse(p)

        def exe_match(p: ParserContext, bytes: Array[Byte]): BitSet 
        = p.map_pos(bytes).result.positions

        private[this] val map_call: (ParserContext, Int) => BitSet = 
        (p: ParserContext, nsym: Int) => {
            val prev_positions = p.result.positions.clone
            val prev_trees = p.result.trees.clone
            val new_result = p.new_result(BitSet())
            prev_positions.foreach(pos => new_result.merge(lookup(p, nsym, pos).update(prev_trees(pos))))
            p.set_result(new_result).result.positions
        }

        private[this] val lookup: (ParserContext, Int, Int) => Memo = 
        (p: ParserContext, nsym: Int, pos: Int) => {
            val memo = p.lookup(nsym, pos)
            (memo.key == pos) match{
                case true => memo
                case false => call_symbol(p, nsym, pos)
            }
        }

        private[this] val call_symbol:(ParserContext, Int, Int) => Memo =
        (p: ParserContext, nsym: Int, pos: Int)
        => parse(p.set_exp(p.rules(nsym)).set_result(p.new_result(BitSet(pos)))).memo(nsym, pos)

        def map_union(p: ParserContext, lhs: PExp, rhs: PExp): BitSet = {
            p.result.positions = p.result.positions.flatMap(pos => union(p, lhs, rhs, pos))
            p.result.positions
        }

        def union(p: ParserContext, lhs: PExp, rhs: PExp, pos: Int): BitSet = {
            val prev_trees = p.result.trees.clone
            merge(p, parse(p.set_exp(lhs).set_result(p.make_result(pos, prev_trees.clone))).result, parse(p.set_exp(rhs).set_result(p.make_result(pos, prev_trees))).result)
        }

        def merge(p: ParserContext, lhs_result: ParserResult, rhs_result: ParserResult): BitSet = {
            (lhs_result.positions.nonEmpty, rhs_result.positions.nonEmpty) match{
                case (true, false) => p.set_result(lhs_result).result.positions
                case (false, true) => p.set_result(rhs_result).result.positions
                case (true, true) => p.set_result(lhs_result.merge(rhs_result)).result.positions
                case (false, false) => p.set_result(p.new_result(BitSet())).result.positions
            }
        }

        private[this] val parse: ParserContext => ParserContext = 
        (p: ParserContext) => {
            p.exp match {
                case PSucc() => p
                case PEmpty(next) => parse(p.set_exp(next))
                case PFail(msg) => throw new RuntimeException(msg)
                case PMatch(bytes, next) => if(!exe_match(p, bytes).nonEmpty) p.set_exp(PFail("")).set_result(p.new_result(BitSet())) else parse(p.set_exp(next))
                case PCallNum(nsym, next) => if(!map_call(p, nsym).nonEmpty) p.set_exp(PFail("")).set_result(p.new_result(BitSet())) else parse(p.set_exp(next))
                case PUnion(lhs, rhs) => if(!map_union(p, lhs, rhs).nonEmpty) p.set_exp(PFail("")) else p
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
                                            rhs_p.HashSetExp(next)
                            amb_parse(lhs_tree, lhs_p)
                        }
                    }
                }
                */
                
                /**
                case PNot(body, next) => Set
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
                            lhs.SetExp(PFail("Match PExp: " + body))
                            rhs.SetExp(PFail("Match PExp: " + body))
                            amb_parse(tree, new_p)
                        }
                    }
                }

                case PAnd(body, next) => Set
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
                            lhs.SetExp(next)
                            rhs.SetExp(next)
                            amb_parse(tree, new_p)
                        }
                    }
                }

                case PMany(body, next) => Set
                    p.exp = body
                    val (new_tree, new_p) = many(tree, p)
                    amb_parse(new_tree, new_p.SetExp(next))
                }

                case PFold(name, body, rec, next) => Set
                    if(p.folding){
                        p.exp = PFold(name, body, rec, next)
                        (tree, p)
                    }else{
                        val (new_tree, new_p) = fold(name, body, rec, tree, p.SetFold(true))
                        amb_parse(new_tree, new_p.SetExp(next).SetFold(false))
                    }
                }
                */
            }
        }        
    }
}