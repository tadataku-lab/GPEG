import AST._

object RemoveLeftRecursion {
    var rules_map = Map.empty[Symbol, PExp]
    //var keys = Set.empty[Symbol]
    //var new_rules = List.empty[(Symbol, PExp)]

    class RLRContext(_keys: Set[Symbol], _new_rules: List[(Symbol, PExp)]){
        var keys = _keys
        var new_rules = _new_rules
        def copy(): RLRContext = {
            new RLRContext(keys, new_rules)
        }
    }
    
    def rem_left_rec(rules: List[(Symbol, PExp)]): List[(Symbol, PExp)] = {
        rules_map = rules.toMap
        val rlr_context = new RLRContext(rules_map.keySet, List.empty[(Symbol, PExp)]) 
        while(rlr_context.keys.nonEmpty){
            rules_map.get(rlr_context.keys.head) match {
                case Some(pexp) => {
                    remove((rlr_context.keys.head, pexp), rlr_context.keys.head, rlr_context)
                    //println(rlr_context.new_rules)
                    rlr_context.keys = rlr_context.keys.tail
                }
                case None => throw new RuntimeException("Rules Error: Don't find rule of symbol " + rlr_context.keys.head)
            }
        }
        rlr_context.new_rules
    }

    def remove(rule: (Symbol, PExp), nonterm: Symbol, r: RLRContext): Unit = {
        rule._2 match {
            case pexp: PCall => {
                if (pexp.name == nonterm){
                    throw new RuntimeException("SyntaxError: Don't remove left recursion")
                }else {
                    rules_map.get(pexp.name) match {
                        case Some(_pexp) => {
                            r.new_rules = r.new_rules :+ rule
                            remove((pexp.name,_pexp), nonterm, r)
                            r.keys = r.keys - pexp.name
                        }
                        case None => throw new RuntimeException("Rules Error: Don't find rule of symbol " + pexp.name)
                    }
                }
            }
            
            case pexp: PIf => {
                val (new_pexp, new_r) = remove_if(pexp, nonterm, r)
                new_r.new_rules = new_r.new_rules :+ ((rule._1, new_pexp))
            }
            
            case pexp: PUnion => {
                println(r.new_rules)
                val (new_pexp, new_r) = remove_union(pexp, nonterm, r)
                new_r.new_rules = new_r.new_rules :+ ((rule._1, new_pexp))
                println(new_r.new_rules)
            }
            
            case pexp: PNot => {
                val (new_pexp, new_r) = remove_body(pexp.body, nonterm, r)
                new_r.new_rules = new_r.new_rules :+ ((rule._1, PNot(new_pexp, pexp.next)))
            }
            case pexp: PAnd => {
                val (new_pexp, new_r) = remove_body(pexp.body, nonterm, r)
                new_r.new_rules = new_r.new_rules :+ ((rule._1, PAnd(new_pexp, pexp.next)))
            }
            case pexp: PMany => {
                val (new_pexp, new_r) = remove_body(pexp.body, nonterm, r)
                new_r.new_rules = new_r.new_rules :+ ((rule._1, PMany(new_pexp, pexp.next)))
            }
            
            case _ => {
                r.new_rules = r.new_rules :+ rule
            }
        }
    }

    def remove_body(body: PExp, nonterm: Symbol, r: RLRContext): (PExp, RLRContext) = {
        body match {
            case pexp: PCall => {
                if (pexp.name == nonterm){
                    throw new RuntimeException("SyntaxError: Don't remove left recursion")
                }else {
                    rules_map.get(pexp.name) match {
                        case Some(_pexp) => {
                            remove((pexp.name,_pexp), nonterm, r)
                            r.keys = r.keys - pexp.name
                            (body, r)
                        }
                        case None => throw new RuntimeException("Rules Error: Don't find rule of symbol " + pexp.name)
                    }
                }
            }
            
            case pexp: PIf => {
                remove_if(pexp, nonterm, r)
            }
            
            case pexp: PUnion => {
                remove_union(pexp, nonterm, r)
            }
            
            case pexp: PNot => {
                remove_body(pexp.body, nonterm, r)
            }
            case pexp: PAnd => {
                remove_body(pexp.body, nonterm, r)
            }
            case pexp: PMany => {
                remove_body(pexp.body, nonterm, r)
            }
            
            case _ => {
                (body, r)
            }
        }
    }
/**
    def remove_if(pexp: PIf, nonterm: Symbol, r: RLRContext): (PIf, RLRContext) = {
        pexp.lhs match {
            case lhs_pexp: PCall => {
                if (lhs_pexp.name == nonterm){
                    pexp.rhs match {
                        case rhs_pexp: PCall => {
                            if (rhs_pexp.name == nonterm){
                                throw new RuntimeException("SyntaxError: Don't remove left recursion")
                            } else { 
                                rules_map.get(rhs_pexp.name) match {
                                    case Some(_pexp) => {
                                        remove((rhs_pexp.name,_pexp), nonterm, r)
                                        r.keys = r.keys - rhs_pexp.name
                                        (PIf(PCall(rhs_pexp.name , lhs_pexp.next), rhs_pexp, pexp.next), r)
                                    }
                                    case None => throw new RuntimeException("Rules Error: Don't find rule of symbol " + rhs_pexp.name)
                                }
                            }
                        }

                        case rhs_pexp: PIf => {
                            val new_symbol = Symbol(lhs_pexp.name.name + "'")
                            val (new_pexp, new_r) = remove_if(isPIf(setSym(rhs_pexp, nonterm ,new_symbol)), new_symbol, r)
                            new_r.new_rules = new_r.new_rules :+ ((new_symbol, new_pexp))
                            (PIf(PCall(new_symbol, lhs_pexp.next), PCall(new_symbol, PSucc()), pexp.next), r)
                        }

                        case rhs_pexp: PUnion => {
                            val new_symbol = Symbol(lhs_pexp.name.name + "'")
                            val (new_pexp, new_r) = remove_union(isPUnion(setSym(rhs_pexp, nonterm ,new_symbol)), new_symbol, r)
                            new_r.new_rules = new_r.new_rules :+ ((new_symbol, new_pexp))
                            (PIf(PCall(new_symbol, lhs_pexp.next), PCall(new_symbol, PSucc()), pexp.next), new_r)
                        }

                        case rhs_pexp: PNot => {
                            val (new_pexp, new_r) = remove_body(rhs_pexp.body, nonterm, r)
                            (PIf(PNot(new_pexp, rhs_pexp.next.set_next(lhs_pexp.next)), PNot(new_pexp, rhs_pexp.next), pexp.next), new_r)
                        }

                        case rhs_pexp: PAnd => {
                            val (new_pexp, new_r) = remove_body(rhs_pexp.body, nonterm, r)
                            (PIf(PAnd(new_pexp, rhs_pexp.next.set_next(lhs_pexp.next)), PAnd(new_pexp, rhs_pexp.next), pexp.next), new_r)
                        }

                        case rhs_pexp: PMany => {
                            val (new_pexp, new_r) = remove_body(rhs_pexp.body, nonterm, r)
                            (PIf(PMany(new_pexp, rhs_pexp.next.set_next(lhs_pexp.next)), PMany(new_pexp, rhs_pexp.next), pexp.next), new_r)
                        }

                        case _ => {
                            (PIf(pexp.rhs.set_next(lhs_pexp.next), pexp.rhs, pexp.next), r)
                        }
                    }
                }else {
                    rules_map.get(lhs_pexp.name) match {
                        case Some(_pexp) => {
                            remove((lhs_pexp.name,_pexp), nonterm, r)
                            r.keys = r.keys - lhs_pexp.name
                            (pexp, r)
                        }
                        case None => throw new RuntimeException("Rules Error: Don't find rule of symbol " + lhs_pexp.name)
                    }
                }
            }
            
            case lhs_pexp: PIf => {
                val (new_pexp, new_r) = remove_if(lhs_pexp, nonterm, r)
                (PIf(new_pexp, pexp.rhs, pexp.next), new_r)
            }
            
            case lhs_pexp: PUnion => {
                val (new_pexp, new_r) = remove_union(lhs_pexp, nonterm, r)
                (PIf(new_pexp, pexp.rhs, pexp.next), new_r)
            }
            
            case lhs_pexp: PNot => {
                val (new_pexp, new_r) = remove_body(lhs_pexp.body, nonterm, r)
                (PIf(new_pexp, pexp.rhs, pexp.next), new_r)
            }
            case lhs_pexp: PAnd => {
                val (new_pexp, new_r) = remove_body(lhs_pexp.body, nonterm, r)
                (PIf(new_pexp, pexp.rhs, pexp.next), new_r)
            }
            case lhs_pexp: PMany => {
                val (new_pexp, new_r) = remove_body(lhs_pexp.body, nonterm, r)
                (PIf(new_pexp, pexp.rhs, pexp.next), new_r)
            }
            
            case _ => {
                (pexp, r)
            }
        }
    }
*/
    def remove_if(pexp: PIf, nonterm: Symbol, r: RLRContext): (PExp, RLRContext) = {
        pexp.lhs match {
            case lhs_pexp: PCall => {
                if (lhs_pexp.name == nonterm){
                    pexp.rhs match {
                        case rhs_pexp: PCall => {
                            if (rhs_pexp.name == nonterm){
                                throw new RuntimeException("SyntaxError: Don't remove left recursion")
                            } else { 
                                rules_map.get(rhs_pexp.name) match {
                                    case Some(_pexp) => {
                                        remove((rhs_pexp.name,_pexp), nonterm, r)
                                        r.keys = r.keys - rhs_pexp.name
                                        (PCall(rhs_pexp.name , PFold(nonterm, remCall(lhs_pexp.next, nonterm), rhs_pexp.name, pexp.next)), r)
                                    }
                                    case None => throw new RuntimeException("Rules Error: Don't find rule of symbol " + rhs_pexp.name)
                                }
                            }
                        }

                        case rhs_pexp: PIf => {
                            val new_symbol = Symbol(lhs_pexp.name.name + "'")
                            val (new_pexp, new_r) = remove_if(isPIf(setSym(rhs_pexp, nonterm ,new_symbol)), new_symbol, r)
                            new_r.new_rules = new_r.new_rules :+ ((new_symbol, new_pexp))
                            (PCall(new_symbol, PFold(nonterm, remCall(lhs_pexp.next, nonterm), new_symbol, pexp.next)), new_r)
                        }

                        case rhs_pexp: PUnion => {
                            val new_symbol = Symbol(lhs_pexp.name.name + "'")
                            val (new_pexp, new_r) = remove_union(isPUnion(setSym(rhs_pexp, nonterm ,new_symbol)), new_symbol, r)
                            new_r.new_rules = new_r.new_rules :+ ((new_symbol, new_pexp))
                            (PCall(new_symbol, PFold(nonterm, remCall(lhs_pexp.next, nonterm), new_symbol, pexp.next)), new_r)
                        }

                        case rhs_pexp: PNot => {
                            val (new_pexp, new_r) = remove_body(rhs_pexp.body, nonterm, r)
                            (PNot(new_pexp, rhs_pexp.next.set_next(PFold(nonterm, remCall(lhs_pexp.next, nonterm), nonterm, pexp.next))), new_r)
                        }

                        case rhs_pexp: PAnd => {
                            val (new_pexp, new_r) = remove_body(rhs_pexp.body, nonterm, r)
                            (PAnd(new_pexp, rhs_pexp.next.set_next(PFold(nonterm, remCall(lhs_pexp.next, nonterm), nonterm, pexp.next))), new_r)
                        }

                        case rhs_pexp: PMany => {
                            val (new_pexp, new_r) = remove_body(rhs_pexp.body, nonterm, r)
                            (PMany(new_pexp, rhs_pexp.next.set_next(PFold(nonterm, remCall(lhs_pexp.next, nonterm), nonterm, pexp.next))), new_r)
                        }

                        case _ => {
                            (pexp.rhs.set_next(PFold(nonterm, remCall(lhs_pexp.next, nonterm), nonterm, pexp.next)), r)
                        }
                    }
                }else {
                    rules_map.get(lhs_pexp.name) match {
                        case Some(_pexp) => {
                            remove((lhs_pexp.name,_pexp), nonterm, r)
                            r.keys = r.keys - lhs_pexp.name
                            (pexp, r)
                        }
                        case None => throw new RuntimeException("Rules Error: Don't find rule of symbol " + lhs_pexp.name)
                    }
                }
            }
            
            case lhs_pexp: PIf => {
                val (new_pexp, new_r) = remove_if(lhs_pexp, nonterm, r)
                (PIf(new_pexp, pexp.rhs, pexp.next), new_r)
            }
            
            case lhs_pexp: PUnion => {
                val (new_pexp, new_r) = remove_union(lhs_pexp, nonterm, r)
                (PIf(new_pexp, pexp.rhs, pexp.next), new_r)
            }
            
            case lhs_pexp: PNot => {
                val (new_pexp, new_r) = remove_body(lhs_pexp.body, nonterm, r)
                (PIf(new_pexp, pexp.rhs, pexp.next), new_r)
            }
            case lhs_pexp: PAnd => {
                val (new_pexp, new_r) = remove_body(lhs_pexp.body, nonterm, r)
                (PIf(new_pexp, pexp.rhs, pexp.next), new_r)
            }
            case lhs_pexp: PMany => {
                val (new_pexp, new_r) = remove_body(lhs_pexp.body, nonterm, r)
                (PIf(new_pexp, pexp.rhs, pexp.next), new_r)
            }
            
            case _ => {
                (pexp, r)
            }
        }
    }


    def remove_union(pexp: PUnion, nonterm: Symbol, r: RLRContext): (PUnion, RLRContext) = {
        pexp.lhs match {
            case lhs_pexp: PCall => {
                if (lhs_pexp.name == nonterm){
                    pexp.rhs match {
                        case rhs_pexp: PCall => {
                            if (rhs_pexp.name == nonterm){
                                throw new RuntimeException("SyntaxError: Don't remove left recursion")
                            } else { 
                                rules_map.get(rhs_pexp.name) match {
                                    case Some(_pexp) => {
                                        remove((rhs_pexp.name,_pexp), nonterm, r)
                                        r.keys = r.keys - rhs_pexp.name
                                        (PUnion(PCall(rhs_pexp.name , lhs_pexp.next), rhs_pexp), r)
                                    }
                                    case None => throw new RuntimeException("Rules Error: Don't find rule of symbol " + rhs_pexp.name)
                                }
                            }
                        }

                        case rhs_pexp: PIf => {
                            val new_symbol = Symbol(lhs_pexp.name.name + "'")
                            val (new_pexp, new_r) = remove_if(isPIf(setSym(rhs_pexp, nonterm ,new_symbol)), new_symbol, r)
                            new_r.new_rules = new_r.new_rules :+ ((new_symbol, new_pexp))
                            (PUnion(PCall(new_symbol, lhs_pexp.next), PCall(new_symbol, PSucc())), new_r)
                        }

                        case rhs_pexp: PUnion => {
                            val new_symbol = Symbol(lhs_pexp.name.name + "'")
                            val (new_pexp, new_r) = remove_union(isPUnion(setSym(rhs_pexp, nonterm ,new_symbol)), new_symbol, r)
                            new_r.new_rules = new_r.new_rules :+ ((new_symbol, new_pexp))
                            println(new_r.new_rules)
                            (PUnion(PCall(new_symbol, lhs_pexp.next), PCall(new_symbol, PSucc())), new_r)
                        }

                        case rhs_pexp: PNot => {
                            val (new_pexp, new_r) = remove_body(rhs_pexp.body, nonterm, r)
                            (PUnion(PNot(new_pexp, rhs_pexp.next.set_next(lhs_pexp.next)), PNot(new_pexp, rhs_pexp.next)), new_r)
                        }

                        case rhs_pexp: PAnd => {
                            val (new_pexp, new_r) = remove_body(rhs_pexp.body, nonterm, r)
                            (PUnion(PAnd(new_pexp, rhs_pexp.next.set_next(lhs_pexp.next)), PAnd(new_pexp, rhs_pexp.next)), new_r)
                        }

                        case rhs_pexp: PMany => {
                            val (new_pexp, new_r) = remove_body(rhs_pexp.body, nonterm, r)
                            (PUnion(PMany(new_pexp, rhs_pexp.next.set_next(lhs_pexp.next)), PMany(new_pexp, rhs_pexp.next)), new_r)
                        }

                        case _ => {
                            (PUnion(pexp.rhs.set_next(lhs_pexp.next), pexp.rhs), r)
                        }
                    }
                }else {
                    rules_map.get(lhs_pexp.name) match {
                        case Some(_pexp) => {
                            remove((lhs_pexp.name,_pexp), nonterm, r)
                            r.keys = r.keys - lhs_pexp.name
                            (pexp, r)
                        }
                        case None => throw new RuntimeException("Rules Error: Don't find rule of symbol " + lhs_pexp.name)
                    }
                }
            }
            
            case lhs_pexp: PIf => {
                val (new_pexp, new_r) = remove_if(lhs_pexp, nonterm, r)
                (PUnion(new_pexp, pexp.rhs), new_r)
            }
            
            case lhs_pexp: PUnion => {
                val (new_pexp, new_r) = remove_union(lhs_pexp, nonterm, r)
                (PUnion(new_pexp, pexp.rhs), new_r)
            }
            
            case lhs_pexp: PNot => {
                val (new_pexp, new_r) = remove_body(lhs_pexp.body, nonterm, r)
                (PUnion(new_pexp, pexp.rhs), new_r)
            }
            case lhs_pexp: PAnd => {
                val (new_pexp, new_r) = remove_body(lhs_pexp.body, nonterm, r)
                (PUnion(new_pexp, pexp.rhs), new_r)
            }
            case lhs_pexp: PMany => {
                val (new_pexp, new_r) = remove_body(lhs_pexp.body, nonterm, r)
                (PUnion(new_pexp, pexp.rhs), new_r)
            }
            
            case _ => {
                (pexp, r)
            }
        }
    }

    def setSym(pexp: PExp, old_sym: Symbol, new_sym: Symbol): PExp = {
        pexp match {
            case _pexp: PCall => {
                if(_pexp.name == old_sym){
                    PCall(new_sym, setSym(_pexp.next, old_sym, new_sym))
                }else{
                    PCall(_pexp.name, setSym(_pexp.next, old_sym, new_sym))
                }
            }

            case _pexp: PEmpty => {
                PEmpty(setSym(_pexp.next, old_sym, new_sym))
            }

            case _pexp: PAny => {
                PAny(setSym(_pexp.next, old_sym, new_sym))
            }

            case _pexp: PMatch => {
                PMatch(_pexp.bytes, setSym(_pexp.next, old_sym, new_sym))
            }

            case _pexp: PIf => {
                PIf(setSym(_pexp.lhs, old_sym, new_sym), setSym(_pexp.rhs, old_sym, new_sym), setSym(_pexp.next, old_sym, new_sym))
            }

            case _pexp: PUnion => {
                PUnion(setSym(_pexp.lhs, old_sym, new_sym), setSym(_pexp.rhs, old_sym, new_sym))
            }

            case _pexp: PNot => {
                PNot(_pexp.body, setSym(_pexp.next, old_sym, new_sym))
            }

            case _pexp: PAnd => {
                PAnd(_pexp.body, setSym(_pexp.next, old_sym, new_sym))
            }

            case _pexp: PMany => {
                PMany(_pexp.body, setSym(_pexp.next, old_sym, new_sym))
            }

            case _ => pexp
        }
    }

    def remCall(pexp: PExp, name: Symbol): PExp = {
        pexp match {
            case PCall(_name, next) => {
                if(name == _name) PSucc() else PCall(_name, remCall(next, name))
            }
            case _pexp: HasNext => {
                _pexp.assign_next(remCall(_pexp.next, name))
            }
            case PUnion(lhs, rhs) => {
                PUnion(remCall(lhs, name), remCall(rhs, name))
            }
            case _pexp: PFail => _pexp
            case _pexp: PSucc => _pexp
        }
    }

    def isPIf(pexp: PExp): PIf = {
        pexp match {
            case _pexp: PIf => {
                _pexp
            }
            case _ => throw new RuntimeException("This type is not PIf ")
        }
    }

    def isPUnion(pexp: PExp): PUnion = {
        pexp match {
            case _pexp: PUnion => {
                _pexp
            }
            case _ => throw new RuntimeException("This type is not PIf ")
        }
    }

}
