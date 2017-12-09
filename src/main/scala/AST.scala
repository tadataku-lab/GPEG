object Ast {
  trait HasPosition { def pos: Pos }
  case class Pos(line: Int, column: Int)
  case class Grammar(pos: Pos, start: Symbol, rules: Map[Symbol,Exp]) extends HasPosition // G
  //dcase class Rule(pos: Pos, name: Symbol, body: Exp) extends HasPosition // R
  sealed trait Exp extends HasPosition
  case class Empty(pos: Pos, emp: Unit) extends Exp // 'ε'
  case class CharClass(pos: Pos, elems: List[CharClassElement]) extends Exp // 'a,b,c...'
  case class Wildcard(pos: Pos) extends Exp // '.' only gpeg ?
  case class AnyNonterminal(pos: Pos, name: Symbol) extends Exp // 'A,B,C...'
  case class Seq(pos: Pos, lhs: Exp, rhs: Exp) extends Exp // 'e e'
  case class Choice(pos: Pos, lhs: Exp, rhs: Exp) extends Exp // 'e/e'
  case class Alt(pos: Pos, lhs: Exp, rhs: Exp) extends Exp // 'e|e' only gpeg
  case class Rep(pos: Pos, body: Exp) extends Exp // 'e*'
  case class Not(pos: Pos, body: Exp) extends Exp // '!e'
  case class Caputure(pos: Pos, label: Symbol, body: Exp) extends Exp // 'L{e}' only cpeg 
  case class LeftFolding(pos: Pos, label: Symbol, lhs: Exp, rhs: Exp) extends Exp //e |L{e} only cpeg
  case class Str(pos: Pos, target: String) extends Exp
  sealed trait CharClassElement
  case class OneChar(ch: Char) extends CharClassElement
  case class CharRange(from: Char, to: Char) extends CharClassElement
}