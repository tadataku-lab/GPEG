 
object AST {

  case class Grammar(start: Symbol, rules: List[(Symbol,Exp)])
  sealed trait Exp 
  case class Empty() extends Exp
  case class Any() extends Exp
  case class AnyChar(body: Char) extends Exp
  case class NonTerm(name: Symbol) extends Exp 
  case class Seq(lhs: Exp, rhs: Exp) extends Exp
  case class Choice(lhs: Exp, rhs: Exp) extends Exp
  case class Alt(lhs: Exp, rhs: Exp) extends Exp 
  case class Many(body: Exp) extends Exp 
  case class Not(body: Exp) extends Exp 
  case class And(body: Exp) extends Exp
  case class Caputure(label: Symbol, body: Exp) extends Exp
  case class FoldMany(label: Symbol, lhs: Exp, rhs: Exp) extends Exp 
  case class LinkTree(label: Symbol, body: Exp) extends Exp

  case class PGrammar(start: Symbol, rules: Map[Symbol,PExp])
  sealed trait PExp
  case class PSucc() extends PExp
  case class PFail(msg: String) extends PExp
  case class PMatch(bytes: Array[Byte], next: PExp) extends PExp{
    override def toString: String = {
      "PMatch(" + (bytes.map(_.toChar)).mkString + "," + next.toString +")"
    }
  }
  case class PCall(name: Symbol, next: PExp) extends PExp
  case class PIf(lhs: PExp, rhs: PExp, next: PExp) extends PExp
  case class PUnion(lhs: PExp, rhs: PExp) extends PExp
  case class PNot(body: PExp, next: PExp) extends PExp
  case class PAnd(body: PExp, next: PExp) extends PExp
  case class PMany(body: PExp, next: PExp) extends PExp
  case class PCons(name: Symbol, body: PExp, next: PExp) extends PExp
  case class PFold(name: Symbol, body: PExp, rec: PExp, next: PExp) extends PExp
  case class PLink(name: Symbol, body: PExp, next: PExp) extends PExp

}