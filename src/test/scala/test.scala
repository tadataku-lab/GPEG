import org.scalatest._
import java.io._
import GpegParser._
import PackratParser._
import Main._

class TestParse extends FunSuite {

  test("test1") {
    assert(gpeg_parse(new FileReader("src/test/resources/GPEG/test1.gpeg")).toString == file2string("src/test/resources/AST/Grammar/test1.ast"))
  }

  test("test2") {
    assert(gpeg_parse(new FileReader("src/test/resources/GPEG/test2.gpeg")).toString == file2string("src/test/resources/AST/Grammar/test2.ast"))
  }

  test("test3") {
    assert(gpeg_parse(new FileReader("src/test/resources/GPEG/test3.gpeg")).toString == file2string("src/test/resources/AST/Grammar/test3.ast"))
  }

  test("test4") {
    assert(gpeg_parse(new FileReader("src/test/resources/GPEG/test4.gpeg")).toString == file2string("src/test/resources/AST/Grammar/test4.ast"))
  }
/**
  test("math1") {
    val g = gpeg_parse(new FileReader("src/test/resources/GPEG/math.gpeg"))
    assert(g.toString == file2string("src/test/resources/AST/Grammar/math.ast"))
    val pg = CPS.toContinuation(g)
    val result = peg_parse(pg,"1*2+12");
    result match {
      case Some(body) => assert(body._1.toString == file2string("src/test/resources/ParseResult/math1.result"))
      case None => println("can't parse")
    }
  }

  test("math2") {
    val g = gpeg_parse(new FileReader("src/test/resources/GPEG/math.gpeg"))
    assert(g.toString == file2string("src/test/resources/AST/Grammar/math.ast"))
    val pg = CPS.toContinuation(g)
    val result = peg_parse(pg,"1+1");
    result match {
      case Some(body) => assert(body._1.toString == file2string("src/test/resources/ParseResult/math2.result"))
      case None => println("can't parse")
    }
  }

  test("math3.0") {
    val g = gpeg_parse(new FileReader("src/test/resources/GPEG/math.gpeg"))
    assert(g.toString == file2string("src/test/resources/AST/Grammar/math.ast"))
    val pg = CPS.toContinuation(g)
    val result = peg_parse(pg,"((((((((((((((1))))))))))))))");
    result match {
      case Some(body) => assert(body._1.toString == file2string("src/test/resources/ParseResult/math3.result"))
      case None => println("can't parse")
    }
  }

  test("memo3") {
    val g = gpeg_parse(new FileReader("src/test/resources/GPEG/memo.gpeg"))
    val pg = CPS.toContinuation(g)
    val result = peg_parse(pg,"((((((((((((((1))))))))))))))");
    result match {
      case Some(body) => assert(body._1.toString == file2string("src/test/resources/ParseResult/memo1.result"))
      case None => println("can't parse")
    }
  }
  */

  test("highly ambiguity [b2]") {
    val g = gpeg_parse(new FileReader("src/test/resources/GPEG/high_amb.gpeg"))
    val pg = CPS.toContinuation(g)
    val result = peg_parse(pg,"bb");
    result match {
      case Some(body) => assert(body._1.toString == file2string("src/test/resources/ParseResult/b2.result"))
      case None => println("can't parse")
    }
  }

  test("highly ambiguity [b4]") {
    val g = gpeg_parse(new FileReader("src/test/resources/GPEG/high_amb.gpeg"))
    val pg = CPS.toContinuation(g)
    val result = peg_parse(pg,"bbbb");
    result match {
      case Some(body) => assert(body._1.toString == file2string("src/test/resources/ParseResult/b4.result"))
      case None => println("can't parse")
    }
  }

  test("highly ambiguity [b6]") {
    val g = gpeg_parse(new FileReader("src/test/resources/GPEG/high_amb.gpeg"))
    val pg = CPS.toContinuation(g)
    val result = peg_parse(pg,"bbbbbb");
    result match {
      case Some(body) => assert(body._1.toString == file2string("src/test/resources/ParseResult/b6.result"))
      case None => println("can't parse")
    }
  }

}