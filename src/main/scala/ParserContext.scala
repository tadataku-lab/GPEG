import AST._
import Tree._
import scala.collection.mutable.{HashMap, ArrayBuffer, Set}

object ParserContext {
    class ParserContext(_exp: PExp, _rules: Map[Symbol, PExp], _input: Array[Byte]){
        var result: ParserResult = ParserResult(Set(0), Array.fill(_input.length + 1)(ArrayBuffer.empty[Tree]))
        var exp: PExp = _exp
        var folding: Boolean = false
        var ID: Long = 0
        val rules: Map[Symbol, PExp] = _rules
        val input: Array[Byte] = _input
        val input_length: Int = _input.length
        var memo: HashMap[(Symbol, Int), ParserResult] = new HashMap[(Symbol, Int),ParserResult]
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

        def new_result(pos: Int): ParserResult = {
            ParserResult(Set(pos), Array.fill(_input.length + 1)(ArrayBuffer.empty[Tree]))
        }

        def set_exp(e: PExp): ParserContext = {
            exp = e
            this
        }

        def set_result(r: ParserResult): ParserContext = {
            result = r
            this
        }

        def map_pos(bytes: Array[Byte]): ParserContext = {
            result.positions = result.positions.flatMap(i => match_bytes(bytes, i))
            this
        }

        def match_bytes(bytes: Array[Byte], pos: Int): Set[Int] = {
            if(bytesEq(bytes, pos, bytes.length)) result.newLeaf(pos, bytes.length, (bytes.map(_.toChar)).mkString) else Set()
        }

        def bytesEq(bytes: Array[Byte], pos: Int, length: Int): Boolean = {
            if((length + pos) > input_length) false else bytes.sameElements(input.slice(pos, pos + length))
        }

        def makeAmbNode(name: Symbol): Node = {
            result.positions.size match{
                case 0 => Node(Symbol("fail"), ArrayBuffer())
                case 1 => Node(name, result.getHead)
                case _ => Node(name, result.makeAmb)
            }
        }

        def lookup(symbol: Symbol, pos: Int): Option[ParserResult] = {
            memo.get((symbol, pos))
        }

        def memo(symbol: Symbol, pos: Int): Set[Int] = {
            memo += ((symbol, pos) -> result.copy)
            result.positions
        }
/**
        def update(symbol: Symbol, prev: ArrayBuffer[Tree]): ParserContext = {
            this.set_result(result.update(symbol, prev))
        }
*/
/**
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

    case class ParserResult(var positions: Set[Int], var trees: Array[ArrayBuffer[Tree]]){
        def copy(): ParserResult = {
            ParserResult(positions.clone, trees.clone)
        }
        /**
        def update(symbol: Symbol, prev: ArrayBuffer[Tree]): ParserResult = {
            pos.map(i => trees = prev:+Node(symbol, trees(i)))
            this
        }
        */
        def getHead(): ArrayBuffer[Tree] = {
            trees(positions.head)
        }
        def makeAmb(): ArrayBuffer[Tree] = {
            var ab = ArrayBuffer.empty[Tree]
            for(pos <- positions){
                ab = ab:+Node(Symbol("amb<" + pos + ">"), trees(pos))
            }
            ab
        }
        def newLeaf(pos: Int, len: Int, v: String): Set[Int] = {
            val new_index = pos + len
            trees(new_index) = trees(new_index):+Leaf(v)
            trees(pos) = ArrayBuffer.empty[Tree]
            Set(new_index)
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