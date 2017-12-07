object Ast {
  trait HasPosition { def pos: Pos }
  case class Pos(line: Int, column: Int)
  case class Grammar(pos: Pos, start: Exp, rules: List[Rule]) extends HasPosition // G
  case class Rule(pos: Pos, name: Symbol, body: Exp) extends HasPosition // R
  sealed trait Exp extends HasPosition
  case class AnyChar(pos: Pos, ch: Char) extends Exp // 'a,b,c...'
  case class Wildcard(pos: Pos) extends Exp // '.' only gpeg ?
  case class AnyNonterminal(pos: Pos, name: Symbol) extends Exp // 'A,B,C...'
  case class Seq(pos: Pos, lhs: Exp, rhs: Exp) extends Exp // 'e e'
  case class Choice(pos: Pos, lhs: Exp, rhs: Exp) extends Exp // 'e/e'
  case class Alt(pos: Pos, lhs: Exp, rhs: Exp) extends Exp // 'e|e' only gpeg
  case class Rep(pos: Pos, body: Exp) extends Exp // 'e*'
  case class Not(pos: Pos, body: Exp) extends Exp // '!e'
  case class Caputure(pos: Pos, label: Symbol, body: Exp) extends Exp // 'L{e}' only cpeg 
  case class LeftFolding(pos: Pos, label: Symbol, lhs: Exp, rhs: Exp) extends Exp //e |L{e} only cpeg
}