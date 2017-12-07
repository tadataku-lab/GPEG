import org.scalatest._
import Ast._

class TestParse extends FunSuite {

  test("5a") {
    assert(OpegParser.parse("5a") == MyData(Pos(1,1),'5','a'))
  }

}