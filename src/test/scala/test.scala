import org.scalatest._
import java.io._
import GpegParser._

class TestParse extends FunSuite {

  test("test1") {
    assert(parse(new FileReader("src/test/resources/GPEG/test1.gpeg")).toString == file2string("src/test/resources/GPEG/test1.result"))
  }

  test("test2") {
    assert(parse(new FileReader("src/test/resources/GPEG/test2.gpeg")).toString == file2string("src/test/resources/GPEG/test2.result"))
  }

  test("test3") {
    assert(parse(new FileReader("src/test/resources/GPEG/test3.gpeg")).toString == file2string("src/test/resources/GPEG/test3.result"))
  }

  test("test4") {
    assert(parse(new FileReader("src/test/resources/GPEG/test4.gpeg")).toString == file2string("src/test/resources/GPEG/test4.result"))
  }

  test("math") {
    assert(parse(new FileReader("src/test/resources/GPEG/math.gpeg")).toString == file2string("src/test/resources/GPEG/math.result"))
  }

}