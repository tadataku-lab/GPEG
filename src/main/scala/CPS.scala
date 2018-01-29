import AST._
import RemoveLeftRecursion._

object CPS{
    def toContinuation(target: Grammar):PGrammar = {
        var rules = target.rules.map( rule => (rule._1, transform(rule._2, PSucc())))
        rules = rem_left_rec(rules)
        return PGrammar(target.start, rules.toMap)
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