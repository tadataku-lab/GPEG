 
object AST {

  case class Grammar(start: Symbol, rules: List[(Symbol,Exp)])
  sealed trait Exp 
  case class Empty() extends Exp
  case class Any() extends Exp
  case class AnyChar(body: Char) extends Exp
  case class Str(body: String) extends Exp
  case class NonTerm(name: Symbol) extends Exp 
  case class Seq(lhs: Exp, rhs: Exp) extends Exp
  case class Choice(lhs: Exp, rhs: Exp) extends Exp
  case class Alt(lhs: Exp, rhs: Exp) extends Exp 
  case class Many(body: Exp) extends Exp 
  case class Not(body: Exp) extends Exp 
  case class And(body: Exp) extends Exp
  //case class Caputure(label: Symbol, body: Exp) extends Exp
  //case class FoldMany(label: Symbol, lhs: Exp, rhs: Exp) extends Exp 
  //case class LinkTree(label: Symbol, body: Exp) extends Exp

  case class PGrammar(start: Int, rules: Array[PExp], symbols: Array[Symbol])

  sealed trait PExp{
    def copy(): PExp
    def set_next(new_next: PExp): PExp
  }

  sealed trait HasNext{
    val next: PExp
    def assign_next(new_next: PExp): PExp
  }

  case class PEmpty(next: PExp) extends PExp with HasNext{
    def copy(): PEmpty = {
      PEmpty(next.copy)
    }
    def set_next(new_next: PExp): PEmpty = {
      PEmpty(next.set_next(new_next))
    }
    def assign_next(new_next: PExp): PEmpty = {
      PEmpty(new_next)
    }
  }
  case class PSucc() extends PExp{
    def copy(): PSucc = {
      PSucc()
    }
    def set_next(new_next: PExp): PExp = {
      new_next
    }
  }
  case class PFail(msg: String) extends PExp{
    def copy(): PFail = {
      PFail(msg)
    }
    def set_next(new_next: PExp): PFail = {
      PFail(msg)
    }
  }
  case class PMatch(bytes: Array[Byte], next: PExp) extends PExp with HasNext{
    override def toString: String = {
      "PMatch(" + (bytes.map(_.toChar)).mkString + "," + next.toString +")"
    }
    def copy(): PMatch = {
      PMatch(bytes, next.copy)
    }
    def set_next(new_next: PExp): PMatch = {
      PMatch(bytes, next.set_next(new_next))
    }
    def assign_next(new_next: PExp): PMatch = {
      PMatch(bytes, new_next)
    }
  }
  case class PAny(next: PExp) extends PExp with HasNext{
    def copy(): PAny = {
      PAny(next.copy)
    }
    def set_next(new_next: PExp): PAny = {
      PAny(next.set_next(new_next))
    }
    def assign_next(new_next: PExp): PAny = {
      PAny(new_next)
    }
  }
  case class PCall(name: Symbol, next: PExp) extends PExp with HasNext{
    def copy(): PCall = {
      PCall(name, next.copy)
    }
    def set_next(new_next: PExp): PCall = {
      PCall(name, next.set_next(new_next))
    }
    def assign_next(new_next: PExp): PCall = {
      PCall(name, new_next)
    }
  }
  case class PCallNum(num: Int, next: PExp) extends PExp with HasNext{
    def copy(): PCallNum = {
      PCallNum(num, next.copy)
    }
    def set_next(new_next: PExp): PCallNum = {
      PCallNum(num, next.set_next(new_next))
    }
    def assign_next(new_next: PExp): PCallNum = {
      PCallNum(num, new_next)
    }
  }
  case class PIf(lhs: PExp, rhs: PExp, next: PExp) extends PExp with HasNext{
    def copy(): PIf = {
      PIf(lhs.copy, rhs.copy, next.copy)
    }
    def set_next(new_next: PExp): PIf = {
      PIf(lhs.copy, rhs.copy, next.set_next(new_next))
    }
    def assign_next(new_next: PExp): PIf = {
      PIf(lhs.copy, rhs.copy, new_next)
    }
  }
  case class PUnion(lhs: PExp, rhs: PExp) extends PExp {
    def copy(): PUnion = {
      PUnion(lhs.copy, rhs.copy)
    }
    def set_next(new_next: PExp): PUnion = {
      PUnion(lhs.set_next(new_next), rhs.set_next(new_next))
    }
  }
  case class PNot(body: PExp, next: PExp) extends PExp with HasNext{
    def copy(): PNot = {
      PNot(body.copy, next.copy)
    }
    def set_next(new_next: PExp): PNot = {
      PNot(body.copy, next.set_next(new_next))
    }
    def assign_next(new_next: PExp): PNot = {
      PNot(body.copy, new_next)
    }
  }
  case class PAnd(body: PExp, next: PExp) extends PExp with HasNext{
    def copy(): PAnd = {
      PAnd(body.copy, next.copy)
    }
    def set_next(new_next: PExp): PAnd = {
      PAnd(body.copy, next.set_next(new_next))
    }
    def assign_next(new_next: PExp): PAnd = {
      PAnd(body.copy, new_next)
    }
  }
  case class PMany(body: PExp, next: PExp) extends PExp with HasNext{
    def copy(): PMany = {
      PMany(body.copy, next.copy)
    }
    def set_next(new_next: PExp): PMany = {
      PMany(body.copy, next.set_next(new_next))
    }
    def assign_next(new_next: PExp): PMany = {
      PMany(body.copy, new_next)
    }
  }
  
  //case class PCons(name: Symbol, body: PExp, next: PExp) extends PExp
  case class PFold(name: Symbol, body: PExp, rec: Symbol, next: PExp) extends PExp with HasNext{
    def copy(): PFold = {
      PFold(name, body.copy, rec, next.copy)
    }
    def set_next(new_next: PExp): PFold = {
      PFold(name, body.copy, rec, next.set_next(new_next))
    }
    def assign_next(new_next: PExp): PFold = {
      PFold(name, body.copy, rec, new_next)
    }
  }
  case class PFoldNum(num: Int, body: PExp, rec: Int, next: PExp) extends PExp with HasNext{
    def copy(): PFoldNum = {
      PFoldNum(num, body.copy, rec, next.copy)
    }
    def set_next(new_next: PExp): PFoldNum = {
      PFoldNum(num, body.copy, rec, next.set_next(new_next))
    }
    def assign_next(new_next: PExp): PFoldNum = {
      PFoldNum(num, body.copy, rec, new_next)
    }
  }
  //case class PLink(name: Symbol, body: PExp, next: PExp) extends PExp

}