import org.scalatest._
import java.io._

class TestParse extends FunSuite {

  test("test1") {
    assert(OpegParser.parse(new FileReader("src/test/resources/GPEG/test1.gpeg")).toString == "Grammar(Pos(1,1),'A,List(Rule(Pos(1,1),'A,AnyNonterminal(Pos(1,6),'B))))")
  }

}