 
object Ast {

  case class Grammar(start: Exp, rules: Map[Symbol,Exp]) 
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

  /*
  trait HasPosition { def pos: Pos }
  case class Pos(line: Int, column: Int)
  case class Grammar(pos: Pos, start: Symbol, rules: Map[Symbol,Exp]) extends HasPosition 
  sealed trait Exp extends HasPosition
  case class Empty(pos: Pos, emp: Unit) extends Exp
  case class CharClass(pos: Pos, elems: List[CharClassElement]) extends Exp
  case class Wildcard(pos: Pos) extends Exp 
  case class AnyNonterminal(pos: Pos, name: Symbol) extends Exp 
  case class Seq(pos: Pos, lhs: Exp, rhs: Exp) extends Exp
  case class Choice(pos: Pos, lhs: Exp, rhs: Exp) extends Exp
  case class Alt(pos: Pos, lhs: Exp, rhs: Exp) extends Exp 
  case class Rep(pos: Pos, body: Exp) extends Exp 
  case class Not(pos: Pos, body: Exp) extends Exp 
  case class And(pos: Pos, body: Exp) extends Exp
  case class Caputure(pos: Pos, label: Symbol, body: Exp) extends Exp
  case class FoldMany(pos: Pos, label: Symbol, lhs: Exp, rhs: Exp) extends Exp 
  case class LinkTree(pos: Pos, label: Symbol, body: Exp) extends Exp
  case class Str(pos: Pos, target: String) extends Exp
  sealed trait CharClassElement
  case class OneChar(ch: Char) extends CharClassElement
  case class CharRange(from: Char, to: Char) extends CharClassElement 
  */
}