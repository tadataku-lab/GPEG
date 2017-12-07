import org.scalatest._

class TestParse extends FunSuite {

  test("johnny 121") {
    assert(TestSimpleParser.parse("johnny 121").toString == "Word <johnny> occurs with frequency 121")
  }

}