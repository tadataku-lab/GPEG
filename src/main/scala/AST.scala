 
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
  sealed trait PExp{
    def copy(): PExp
  }
  case class PSucc() extends PExp{
    def copy(): PSucc = {
      PSucc()
    }
  }
  case class PFail(msg: String) extends PExp{
    def copy(): PFail = {
      PFail(msg)
    }
  }
  case class PMatch(bytes: Array[Byte], next: PExp) extends PExp{
    override def toString: String = {
      "PMatch(" + (bytes.map(_.toChar)).mkString + "," + next.toString +")"
    }
    def copy(): PMatch = {
      PMatch(bytes, next.copy)
    }
  }
  case class PAny(next: PExp) extends PExp{
    def copy(): PAny = {
      PAny(next.copy)
    }
  }
  case class PCall(name: Symbol, next: PExp) extends PExp{
    def copy(): PCall = {
      PCall(name, next.copy)
    }
  }
  case class PIf(lhs: PExp, rhs: PExp, next: PExp) extends PExp{
    def copy(): PIf = {
      PIf(lhs.copy, rhs.copy, next.copy)
    }
  }
  case class PUnion(lhs: PExp, rhs: PExp) extends PExp{
    def copy(): PUnion = {
      PUnion(lhs.copy, rhs.copy)
    }
  }
  case class PNot(body: PExp, next: PExp) extends PExp{
    def copy(): PNot = {
      PNot(body.copy, next.copy)
    }
  }
  case class PAnd(body: PExp, next: PExp) extends PExp{
    def copy(): PAnd = {
      PAnd(body.copy, next.copy)
    }
  }
  case class PMany(body: PExp, next: PExp) extends PExp{
    def copy(): PMany = {
      PMany(body.copy, next.copy)
    }
  }
  /**
  case class PCons(name: Symbol, body: PExp, next: PExp) extends PExp
  case class PFold(name: Symbol, body: PExp, rec: PExp, next: PExp) extends PExp
  case class PLink(name: Symbol, body: PExp, next: PExp) extends PExp
  */

}