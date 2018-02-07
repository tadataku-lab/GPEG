import java.io._
import PackratParser._
import scala.io.Source
import CPS._
import AST._
import Forest._
import GpegParser._

object Main{

    val USAGE = """ Usage: run [options] <GPEG file(.gpeg)>  [<input string>]

    Example: 
    run -b rule.gpeg bbb
    
    Options: 
    -u, --usage : Show this usage
    -d, --debug : Debug mode
    -b, --bench : Run a benchmark test
    -f <file path>, --file <file path> : Input file
    -s <file name>, --save <file name> : Save result (src/test/resources/ParseResult/)"""

    case class Command(input: Option[String], gpeg: Option[FileReader], option: Opt)
    case class Opt(bench: Boolean, usage: Boolean, debug: Boolean, file: Option[String], save: Option[String])

    def main(args: Array[String]):Unit = {

        var b = "ab"
        //for(i <- 1 to 20) b += "b"
        exeCommand(getCommand(Array("-d", "src/main/resources/GPEG/rule.gpeg", b)))
        //exeCommand(getCommand(args))
        
        /**
        def fact : Long => Long = n => {
            n match {
                case 0 => 1
                case n if n > 0 => n * fact(n - 1)
            }
        }
        val N = (1 to 15).toList
        val C = N.map( n => fact(2 * n) / ((n + 1) * fact(n) * fact(n)))
        println(N)
        println(C)
        */
        
        
    }

    def isOpt(arg: String): Boolean = {
        arg.startsWith("-")
    }

    def getOpt(args: Array[String]):(Int, Opt) = {
        var i = 0
        var opt = Opt(false, false, false, None, None)
        for(arg <- args){
            isOpt(arg) match {
                case true => arg match{
                    case "-u" | "--usage" => {
                        opt = opt.copy(usage = true)
                        return (i + 1, opt)
                    }
                    case "-b" | "--bench" => opt = opt.copy(bench = true)
                    case "-d" | "--debug" => opt = opt.copy(debug = true)
                    case "-f" | "--file" => {
                        opt = opt.copy(file = Some(args(i + 1)))
                        i += 1
                    }
                    case "-s" | "--save" => {
                        opt = opt.copy(save = Some(args(i + 1)))
                        i += 1
                    }
                    case _ => throw new RuntimeException(arg + " isn't a option.\n" + USAGE)
                    }
                case false => return (i, opt)
            }
            i += 1
        }
        throw new RuntimeException("There is no gpeg file.\n" + USAGE)
    }

    def getCommand(args: Array[String]): Command = {
        val (i, opt) = getOpt(args)
        (args.length - i) match{
            case 0 => new Command( None, None, opt)
            case 1 => new Command( None, Some(new FileReader(args(i))), opt)
            case 2 => new Command( Some(args(i + 1)), Some(new FileReader(args(i))), opt)
            case _ => throw new RuntimeException("Too many arguments.\n" + USAGE)
        }
    }

    def exeCommand(command: Command):Unit = {
        if(command.option.usage) println(USAGE) else exeGrammar(command)
    }

    def exeGrammar(command: Command):Unit = {
        command.gpeg match {
            case None => throw new RuntimeException("There is no arguments.\n" + USAGE)
            case Some(gpeg) => {
                if(command.option.debug){
                    val g = gpeg_parse(gpeg)
                    println(g);
                    val pg = toContinuation(g)
                    println(pg);
                    exeParse(pg, command)
                }else exeParse(toContinuation(gpeg_parse(gpeg)), command)
            }
        }
    }

    def exeParse(pgrammar: PGrammar, command: Command): Unit = {
        command.option.file match {
            case None =>
            case Some(input) => showParseResult(pgrammar, file2string(input), command.option)
        }
        command.input match {
            case None => interpriterMode(pgrammar, command)
            case Some(input) => showParseResult(pgrammar, input, command.option)
        }
    }

    def showParseResult(pgrammar: PGrammar, input: String, option: Opt): Unit = {
        if(option.bench){
            val start = System.currentTimeMillis
            val result = peg_parse(pgrammar,input);
            val time = System.currentTimeMillis - start
            result match {
                case Some(body) => {
                    option.save match {
                        case None => println(body._1)
                        case Some(name) => save_and_show(body._1, name)
                    }
                }
                case None => println("can't parse")
            }
            println(time + "[ms]")              
        }else {
            peg_parse(pgrammar,input) match {
                case Some(body) => {
                    option.save match {
                        case None => println(body._1)
                        case Some(name) => save_and_show(body._1, name)
                    }
                }
                case None => println("can't parse")
            }
        }
    }

    def interpriterMode(pgrammar: PGrammar, command: Command): Unit = {
        println("Please input string")
        while(true){
            print(">>")
            val input = io.StdIn.readLine()
            input match {
                case ":q" | ":quit" => return println("exit.")
                case _input => showParseResult(pgrammar, _input, command.option)
            }
        }
    }

    def save_and_show(forest: Forest, filename: String):Unit = {
        println(forest)
        val file = new PrintWriter("src/test/resources/ParseResult/" + filename)
        file.write(forest.toString())
        file.close()
    }

    def file2string(filename: String): String = {
        val source = Source.fromFile(filename)
        val sb = new StringBuilder
        for( line <- source.getLines ) {
            sb.append(line)
        }
        source.close
        sb.toString
    }
}
