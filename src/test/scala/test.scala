import org.scalatest._
import Ast._

class TestParse extends FunSuite {

  test("A = B") {
    assert(OpegParser.parse("A = B") == Grammar(Pos(1,1),Symbol("A"),List(Rule(Pos(1,1),Symbol("A"),AnyNonterminal(Pos(1,5),Symbol("B"))))))
    assert(OpegParser.parse("A = B").toString == "Grammar(Pos(1,1),'A,List(Rule(Pos(1,1),'A,AnyNonterminal(Pos(1,5),'B))))")
  }

}