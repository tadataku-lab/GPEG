import org.scalatest._
import java.io._

class TestParse extends FunSuite {

  test("test1") {
    assert(GpegParser.parse(new FileReader("src/test/resources/GPEG/test1.gpeg")).toString == "Grammar(Pos(1,1),'A,Map('A -> AnyNonterminal(Pos(1,6),'B)))")
  }

  test("test2") {
    assert(GpegParser.parse(new FileReader("src/test/resources/GPEG/test2.gpeg")).toString == "Grammar(Pos(1,1),'A,Map('A -> AnyNonterminal(Pos(1,6),'B), 'B -> Str(Pos(2,6),Hello)))")
  }

  test("test3") {
    assert(GpegParser.parse(new FileReader("src/test/resources/GPEG/test3.gpeg")).toString == "Grammar(Pos(1,1),'S,Map('S -> Alt(Pos(1,16),Alt(Pos(1,11),AnyNonterminal(Pos(1,5),'SSS),AnyNonterminal(Pos(1,11),'SS)),AnyNonterminal(Pos(1,16),'b))))")
  }

  test("test4") {
    assert(GpegParser.parse(new FileReader("src/test/resources/GPEG/test4.gpeg")).toString == "Grammar(Pos(1,1),'A,Map('A -> CharClass(Pos(1,6),List(CharRange(a,z)))))")
  }

}