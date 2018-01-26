import java.io._
import PackratParser._
import scala.io.Source
import CPS._
import GpegParser._

object Main{

    def main(args: Array[String]):Unit = {
        val test = "\n".getBytes
        println(test(0))
        if(args.length == 0){
            val g2 = gpeg_parse(new FileReader("src/main/resources/GPEG/rule.gpeg"))
            println(g2);
            val pg2 = toContinuation(g2)
            println(pg2);
            //val g = parse(new FileReader("src/main/resources/GPEG/rule.gpeg"))
            val g = gpeg_parse(new FileReader("src/test/resources/GPEG/test7.gpeg"))
            println(g);
            val pg = toContinuation(g)
            println(pg);
            /**
            val source = Source.fromFile("src/main/resources/XML/pom.xml")
            val sb = new StringBuilder
            try{
                for(line <- source.getLines){
                    sb.append(line)
                }
            }finally{
                source.close
            }
            */
        
            val start = System.currentTimeMillis
            //val result = peg_parse(pg,"((((((((((((((1))))))))))))))");
            //val result = peg_parse(pg,"((((1))))");
            //val result = peg_parse(pg,"1*2+12");
            //val result = peg_parse(pg2,"1/4-2-3");
            val ten_b = "bbbbbbbbbb"
            val result = peg_parse(pg2, ten_b + "bb");
            //val result = peg_parse(pg,sb.toString);
            val time = System.currentTimeMillis - start
            /**
            result match {
                case Some(body) => {
                    println("tree: " + body._1)
                    //println("rest: " + body._2)
                }
                case None => println("can't parse")
            }
            */
            println(time + "ms")
        }else if(args.length == 1){
            val g = gpeg_parse(new FileReader("src/main/resources/GPEG/rule.gpeg"))
            val file = new PrintWriter(args(0))
            file.write(g.toString())
            file.close()
        }else{
            val file = new PrintWriter(args(0))
            val g = gpeg_parse(new FileReader(args(1)))
            file.write(g.toString())
            file.close()
        }
    }

    def file2string(filename: String): String = {
        val source = Source.fromFile(filename)
        val sb = new StringBuilder
        for( line <- source.getLines ) {
            sb.append(line)
        }
        source.close
        return sb.toString
    }
}
