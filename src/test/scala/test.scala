import org.scalatest._

class TestParse extends FunSuite {

  test("5a") {
    assert(OpegParser.parse("5a") == MyData('5','a'))
  }

}