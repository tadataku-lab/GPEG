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

  test("math") {
    assert(GpegParser.parse(new FileReader("src/test/resources/GPEG/math.gpeg")).toString == "Grammar(Pos(1,1),'expression,Map('expression -> AnyNonterminal(Pos(1,15),'additive), 'additive -> Seq(Pos(2,23),AnyNonterminal(Pos(2,13),'multitive),Rep(Pos(2,23),Choice(Pos(2,44),Seq(Pos(2,28),Str(Pos(2,24),+),AnyNonterminal(Pos(2,28),'multitive)),Seq(Pos(2,44),Str(Pos(2,40),-),AnyNonterminal(Pos(2,44),'multitive))))), 'multitive -> Seq(Pos(3,20),AnyNonterminal(Pos(3,14),'value),Rep(Pos(3,20),Choice(Pos(3,37),Seq(Pos(3,25),Str(Pos(3,21),*),AnyNonterminal(Pos(3,25),'value)),Seq(Pos(3,37),Str(Pos(3,33),/),AnyNonterminal(Pos(3,37),'value))))), 'value -> Choice(Pos(4,34),Seq(Pos(4,10),CharClass(Pos(4,10),List(CharRange(0,9))),Rep(Pos(4,10),CharClass(Pos(4,10),List(CharRange(0,9))))),Seq(Pos(4,34),Seq(Pos(4,23),Str(Pos(4,19),(),AnyNonterminal(Pos(4,23),'expression)),Str(Pos(4,34),))))))")
  }

}