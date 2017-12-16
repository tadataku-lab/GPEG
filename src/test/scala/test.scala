import org.scalatest._
import java.io._
import GpegParser._
import PegParser._

class TestParse extends FunSuite {

  test("test1") {
    assert(GpegParser.parse(new FileReader("src/test/resources/GPEG/test1.gpeg")).toString == file2string("src/test/resources/GPEG/test1.result"))
  }

  test("test2") {
    assert(GpegParser.parse(new FileReader("src/test/resources/GPEG/test2.gpeg")).toString == file2string("src/test/resources/GPEG/test2.result"))
  }

  test("test3") {
    assert(GpegParser.parse(new FileReader("src/test/resources/GPEG/test3.gpeg")).toString == file2string("src/test/resources/GPEG/test3.result"))
  }

  test("test4") {
    assert(GpegParser.parse(new FileReader("src/test/resources/GPEG/test4.gpeg")).toString == file2string("src/test/resources/GPEG/test4.result"))
  }

  test("math") {
    val g = GpegParser.parse(new FileReader("src/test/resources/GPEG/math.gpeg"))
    assert(g.toString == file2string("src/test/resources/GPEG/math.result"))
    val pg = CPS.toContinuation(g)
    val result = peg_parse(pg,"1*2+12");
    result match {
      case Some(body) => assert(body._1.toString == file2string("src/test/resources/AST/math.ast"))
      case None => println("can't parse")
    }
  }

  test("bytesEq") {
    assert(bytesEq("0123456789".getBytes, "0123456789"))
  }

}