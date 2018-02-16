import AST._
import RemoveLeftRecursion._
import scala.collection.mutable.{OpenHashMap}

object CPS{
    val symMap: OpenHashMap[Symbol, Int] = OpenHashMap.empty[Symbol, Int]

    def toContinuation(target: Grammar):PGrammar = {
        var rules = target.rules.map( rule => (rule._1, transform(rule._2, PSucc())))
        rules = rem_left_rec(rules)
        val symbols = make_symMap(rules)
        val prules = changePCallNum(rules)
        return PGrammar(symMap(target.start), prules, symbols)
    }

    def make_symMap(rules: List[(Symbol, PExp)]): Array[Symbol] = {
        var num = 0
        val symbols: Array[Symbol] = Array.fill(rules.size)(null)
        rules.foreach(rule => { 
            symMap += (rule._1 -> num)
            symbols(num) = rule._1
            num += 1
        })
        symbols
    }

    def changePCallNum(r: List[(Symbol, PExp)]): Array[PExp] = {
        val rules: Array[PExp] = Array.fill(r.size)(null)
        r.foreach(rr => rules(symMap(rr._1)) = make_rules(rr._2))
        rules
    }

    def make_rules(p: PExp): PExp = {
        p match {
            case s: PSucc => s
            case f: PFail => f
            case PEmpty(next) => PEmpty(make_rules(next))
            case PMatch(b,next) => PMatch(b, make_rules(next))
            case PAny(next) => PAny(make_rules(next))
            case PCall(name, next) => PCallNum(symMap(name), make_rules(next))
            case PIf(l, r, next) => PIf(make_rules(l), make_rules(r), make_rules(next))
            case PUnion(l, r) => PUnion(make_rules(l), make_rules(r))
            case PNot(b, next) => PNot(make_rules(b), make_rules(next))
            case PAnd(b, next) => PAnd(make_rules(b), make_rules(next))
            case PMany(b, next) => PMany(make_rules(b), make_rules(next))
            case PFold(name, b, rec, next) => PFoldNum(symMap(name), make_rules(b), symMap(rec), make_rules(next))
        }
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