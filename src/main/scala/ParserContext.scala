import AST._
import Memo._
import Forest._
import scala.collection.mutable.{HashMap}
import ParserContext._
object ParserContext {
    class ParserContext(_exp: PExp, _rules: Map[Symbol, PExp], _input: Array[Byte]){
        var states = List(State(0, List()))
        var exp = _exp
        var folding = false
        val rules: Map[Symbol, PExp] = _rules
        val input: Array[Byte] = _input
        var memo: HashMap[(Symbol, Int), State] = new HashMap[(Symbol, Int),State]

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
            if(states.isEmpty) return Node(Symbol("fail"), List())
            if(states.length == 1) states.head.newNode(name) else Node(Symbol("ambiguity"), states.flatMap(state => List(state.newNode(name))))
        }

    }
    

    case class State(var pos: Int, var trees: List[Tree]){
        def new_state(): State = {
            State(pos, List())
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
    }

    case class Position(pos: Long, id: Long, lrb: LRB)

    sealed trait LRB
    case class Left() extends LRB
    case class Right() extends LRB
    case class Both() extends LRB
}