import AST._
import Tree._
import scala.collection.mutable.{HashMap,ArrayBuffer}

object ParserContext {
    class ParserContext(_exp: PExp, _rules: Map[Symbol, PExp], _input: Array[Byte]){
        var states = ArrayBuffer(State(0, ArrayBuffer()))
        var exp = _exp
        var folding = false
        val rules: Map[Symbol, PExp] = _rules
        val input: Array[Byte] = _input
        var memo: HashMap[(Symbol, Int), ArrayBuffer[State]] = new HashMap[(Symbol, Int),ArrayBuffer[State]]

        def dump_memo(): ParserContext = {
            println("memo: " + memo)
            this
        }

        def dump_states(): ParserContext = {
            println("states: " + states)
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

        def set_states(s: ArrayBuffer[State]): ParserContext = {
            states = s
            this
        }

        def match_bytes(state: State, bytes: Array[Byte]): ArrayBuffer[State] = {
            if(bytesEq(bytes, state.pos, bytes.length)) ArrayBuffer(state.newLeaf((bytes.map(_.toChar)).mkString).newPos(bytes.length)) else ArrayBuffer()
        }

        def bytesEq(bytes: Array[Byte], pos: Int, length: Int): Boolean = {
            if((length + pos) > input.length) false else bytes.sameElements(input.slice(pos, pos + length))
        }

        def newAmbNode(name: Symbol): Node = {
            states.length match{
                case 0 => Node(Symbol("fail"), ArrayBuffer())
                case 1 => states.head.newNode(name)
                case _ => Node(Symbol("ambiguity"), states.flatMap(state => ArrayBuffer(state.newNode(name))))
            }
        }

        def lookup(symbol: Symbol, pos: Int): Option[ArrayBuffer[State]] = {
            memo.get((symbol, pos))
        }

        def memo(symbol: Symbol, pos: Int): ArrayBuffer[State] = {
            memo += ((symbol, pos) -> states.map(state => state.copy))
            states
        }

        def merge():ParserContext = {
            var s = states
            s.length match{
                case 0 => 
                case 1 => 
                case _ => {
                    var new_states = ArrayBuffer.empty[State]
                    while(s.nonEmpty){
                        val state = s.head
                        s = s.tail
                        val (eq, notEq) = s.partition(ss => ss.posEq(state))
                        s = notEq
                        if(eq.nonEmpty){
                            new_states = new_states:+state.merge(eq)
                        }else new_states = new_states:+state
                    }
                    states = new_states
                }
            }
            this
        }

    }
    

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

        def merge(states: ArrayBuffer[State]): State = {
            trees = ArrayBuffer(Node(Symbol("ambiguity"), trees++states.flatMap(state => state.trees)))
            this
        }

        def posEq(state: State): Boolean = {
            pos == state.pos
        }
    }
}