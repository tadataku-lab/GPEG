import AST._

object CPS{
    def toContinuation(target: Grammar):PGrammar = {
        val rules = target.rules.map( rule => (rule._1, transform(rule._2, null)))
        return PGrammar(target.start, rules.toMap)
    }

    def transform(target: Exp, next: Exp): PExp = {
        target match {
            case Seq(lhs,rhs) => transform(lhs, rhs)
            case Empty() => PEmpty(transform(next, null))
            case Any() => PAny(transform(next, null))
            case AnyChar(body) => PMatch(Array(body.toByte), transform(next, null))
            case Str(body) => {
                if(body != ""){
                    PMatch(body.getBytes, transform(next, null))
                }else PEmpty(transform(next, null))
            }
            case NonTerm(symbol) => PCall(symbol, transform(next, null))
            case Choice(lhs, rhs) => PIf(transform(lhs, null), transform(rhs, null), transform(next, null))
            case Alt(lhs, rhs) => PUnion(transform(lhs, next), transform(rhs, next))
            case Many(body) => PMany(transform(body,null), transform(next,null))
            case Not(body) => PNot(transform(body,null), transform(next,null))
            case And(body) => PAnd(transform(body,null), transform(next,null))
            case null => PSucc()
        }
    }

}