import AST._
import Tree._
import scala.collection.mutable.{HashMap}

object ParserContext {
    class ParserContext(_exp: PExp, _rules: Map[Symbol, PExp], _input: Array[Byte]){
        var states = List(State(0, List()))
        var exp = _exp
        var folding = false
        val rules: Map[Symbol, PExp] = _rules
        val input: Array[Byte] = _input
        var memo: HashMap[(Symbol, Int), List[State]] = new HashMap[(Symbol, Int),List[State]]

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

        def set_states(s: List[State]): ParserContext = {
            states = s
            this
        }

        def match_bytes(state: State, bytes: Array[Byte]): List[State] = {
            if(bytesEq(bytes, state.pos, bytes.length)) List(state.newLeaf((bytes.map(_.toChar)).mkString).newPos(bytes.length)) else List()
        }

        def bytesEq(bytes: Array[Byte], pos: Int, length: Int): Boolean = {
            if((length + pos) > input.length) false else bytes.sameElements(input.slice(pos, pos + length))
        }

        def newAmbNode(name: Symbol): Node = {
            states.length match{
                case 0 => Node(Symbol("fail"), List())
                case 1 => states.head.newNode(name)
                case _ => Node(Symbol("ambiguity"), states.flatMap(state => List(state.newNode(name))))
            }
        }

        def lookup(symbol: Symbol, pos: Int): Option[List[State]] = {
            memo.get((symbol, pos))
        }

        def memo(symbol: Symbol, pos: Int): List[State] = {
            println("symbol: " + symbol + " pos: " + pos + " _states: " + states )
            memo += ((symbol, pos) -> states.map(state => state.copy))
            states
        }

        def merge(_states: List[State]):List[State] = {
            var s = _states
            s.length match{
                case 0 => List.empty[State]
                case 1 => s
                case _ => {
                    var new_states = List.empty[State]
                    for(state <- s){
                        val (eq, notEq) = s.partition(ss => ss.posEq(state))
                        if(eq.nonEmpty){
                            s = eq
                            new_states = new_states:+state.merge(eq)
                        }
                    }
                    s:::new_states
                }
            }
        }

    }
    

    case class State(var pos: Int, var trees: List[Tree]){
        def copy(): State = {
            State(pos, trees)
        }

        def newState(): State = {
            State(pos, List())
        }

        def update(name: Symbol, state: State): State = {
            trees = state.trees:+Node(name, trees)
            this
        }

        def newLeaf(v: String): State = {
            trees = trees:+Leaf(v)
            this
        }

        def addNode(name: Symbol, next: List[Tree]): State = {
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

        def merge(states: List[State]): State = {
            trees = trees:+Node(Symbol("ambiguity"), states.flatMap(state => state.trees))
            this
        }

        def posEq(state: State): Boolean = {
            pos == state.pos
        }
    }
}