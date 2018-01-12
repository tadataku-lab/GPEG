import AST._

object CPS{
    def toContinuation(target: Grammar):PGrammar = {
        var rules = target.rules.map( rule => (rule._1, transform(rule._2, PSucc())))
        println(rules)
        rules = rules.map( rule => rem_left_rec(rule._1, rule._2))
        return PGrammar(target.start, rules.toMap)
    }

    def rem_left_rec(nonterm: Symbol, target: PExp): (Symbol, PExp) = {
        var _target = target
        target match {
            case pexp: PCall => {
                if (pexp.name == nonterm){
                    _target = target
                }
            }
            case pexp: PIf => {
                _target = PIf(rem_left_rec(nonterm, pexp.lhs)._2, rem_left_rec(nonterm, pexp.rhs)._2, pexp.next)
            }
            case pexp: PUnion => {
                _target = PUnion(rem_left_rec(nonterm, pexp.lhs)._2, rem_left_rec(nonterm, pexp.rhs)._2)
            }
            case pexp: PNot => {
                _target = PNot(rem_left_rec(nonterm, pexp.body)._2, pexp.next)
            }
            case pexp: PAnd => {
                _target = PAnd(rem_left_rec(nonterm, pexp.body)._2, pexp.next)
            }
            case pexp: PMany => {
                _target = PMany(rem_left_rec(nonterm, pexp.body)._2, pexp.next)
            }
            case _ => {}
        }
        return (nonterm, _target)
    }

    def transform(target: Exp, next: PExp): PExp = {
        target match {
            case Seq(lhs,rhs) => transform(lhs, transform(rhs, next))
            case Empty() => PEmpty(next)
            case Any() => PAny(next)
            case AnyChar(body) => PMatch(Array(body.toByte), next)
            case Str(body) => {
                if(body != ""){
                    PMatch(body.getBytes, next)
                }else PEmpty(next)
            }
            case NonTerm(symbol) => PCall(symbol, next)
            case Choice(lhs, rhs) => PIf(transform(lhs, PSucc()), transform(rhs, PSucc()), next)
            case Alt(lhs, rhs) => PUnion(transform(lhs, next), transform(rhs, next))
            case Many(body) => PMany(transform(body,PSucc()), next)
            case Not(body) => PNot(transform(body,PSucc()), next)
            case And(body) => PAnd(transform(body,PSucc()), next)
        }
    }

}