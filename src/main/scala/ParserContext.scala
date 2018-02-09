import AST._
import Tree._
import scala.collection.mutable.{HashMap,ArrayBuffer}

object ParserContext {
    class ParserContext(_exp: PExp, _rules: Map[Symbol, PExp], _input: Array[Byte]){
        var result: ParserResult = ParserResult(ArrayBuffer(0), Array.fill(_input.length + 1)(ArrayBuffer.empty[Tree]))
        var exp: PExp = _exp
        var folding: Boolean = false
        var ID: Long = 0
        val rules: Map[Symbol, PExp] = _rules
        val input: Array[Byte] = _input
        val input_length: Int = _input.length
        var memo: HashMap[(Symbol, Int), Array[ArrayBuffer[Tree]]] = new HashMap[(Symbol, Int),Array[ArrayBuffer[Tree]]]
        var bench: Array[Long] = Array(0, 0, 0, 0, 0, 0)

        def dump_memo(): ParserContext = {
            println("memo: " + memo)
            this
        }

        def dump_result(): ParserContext = {
            println("result: " + result)
            this
        }

        def dump_exp(): ParserContext = {
            println("exp: " + exp)
            this
        }

        def set_exp(e: PExp): ParserContext = {
            exp = e
            this
        }

        def set_result(r: ParserResult): ParserContext = {
            result = r
            this
        }

        def match_bytes(pos: Int, bytes: Array[Byte]):ArrayBuffer[Int] = {
            if(bytesEq(bytes, pos, bytes.length)) result.newLeaf(pos, bytes.length, (bytes.map(_.toChar)).mkString) else ArrayBuffer()
        }

        def bytesEq(bytes: Array[Byte], pos: Int, length: Int): Boolean = {
            if((length + pos) > input_length) false else bytes.sameElements(input.slice(pos, pos + length))
        }

        def makeAmbNode(name: Symbol): Node = {
            result.pos.length match{
                case 0 => Node(Symbol("fail"), ArrayBuffer())
                case 1 => Node(name, result.getHead)
                case _ => Node(name, result.makeAmb)
            }
        }
/**
        def lookup(symbol: Symbol, pos: Int): Option[ArrayBuffer[State]] = {
            memo.get((symbol, pos))
        }

        def memo(symbol: Symbol, pos: Int): ArrayBuffer[State] = {
            memo += ((symbol, pos) -> states.map(state => state.copy))
            states
        }

        def merge():ParserContext = {
            bench(0) += 1
            val start = System.currentTimeMillis
            var s = states
            s.length match{
                case 0 => 
                case 1 => 
                case _ => {
                    var new_states = ArrayBuffer.empty[State]
                    while(s.nonEmpty){
                        val state = s.head
                        s = s.tail
                        bench(2) += 1
                        val start1 = System.currentTimeMillis
                        val (eq, notEq) = s.partition(ss => ss.posEq(state))
                        val time1 = System.currentTimeMillis - start1
                        bench(3) += time1
                        s = notEq
                        if(eq.nonEmpty){
                            bench(4) += 1
                            val start2 = System.currentTimeMillis
                            new_states = new_states:+state.merge(eq, ID)
                            val time2 = System.currentTimeMillis - start2
                            bench(5) += time2
                            ID += 1
                        }else new_states = new_states:+state
                    }
                    states = new_states
                }
            }
            val time = System.currentTimeMillis - start
            bench(1) += time
            this
        }
*/
    }

    case class ParserResult(var pos: ArrayBuffer[Int], var trees: Array[ArrayBuffer[Tree]]){
        def getHead(): ArrayBuffer[Tree] = {
            trees(pos.head)
        }
        def makeAmb(): ArrayBuffer[Tree] = {
            pos.flatMap(i => ArrayBuffer(Node(Symbol("amb<" + i + ">"), trees(i))))
        }
        def newLeaf(pos: Int, len: Int, v: String): ArrayBuffer[Int] = {
            val new_pos = pos + len
            trees(new_pos) = trees(new_pos):+Leaf(v)
            trees(pos) = ArrayBuffer.empty[Tree]
            ArrayBuffer(new_pos)
        }
    }
    
/**
    case class State(var pos: Int, var trees: ArrayBuffer[Tree]){
        def copy(): State = {
            State(pos, trees)
        }

        def newState(): State = {
            State(pos, ArrayBuffer())
        }

        def update(name: Symbol, state: State): State = {
            trees = state.trees:+Node(name, trees)
            this
        }

        def newLeaf(v: String): State = {
            trees = trees:+Leaf(v)
            this
        }

        def addNode(name: Symbol, next: ArrayBuffer[Tree]): State = {
            trees = trees:+Node(name, next)
            this
        }

        def newNode(name: Symbol): Node = {
            Node(name, trees)
        }

        def newPos(len: Int): State = {
            pos = pos + len
            this
        }

        def merge(states: ArrayBuffer[State], id: Long): State = {
            val sym = Symbol("amb<" + id + ">")
            trees = ArrayBuffer(Node(sym,trees))++states.flatMap(state => ArrayBuffer(Node(sym, state.trees)))
            this
        }

        def posEq(state: State): Boolean = {
            pos == state.pos
        }
    }
    */
}