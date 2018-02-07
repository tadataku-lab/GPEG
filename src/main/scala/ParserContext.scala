import AST._
import Memo._
import Forest._
import scala.collection.mutable.{HashMap}
import ParserContext._
object ParserContext {
    class ParserContext(_exp: PExp, _rules: Map[Symbol, PExp], _input: Array[Byte], _start: Symbol){
        var states = List(State(0, List(), 0, Both()))
        val forest = new Forest()
        var exp = _exp
        var folding = false
        val rules: Map[Symbol, PExp] = _rules
        val input: Array[Byte] = _input
        var ID: Long = 0
        var mapID: Map[Long, Long] = Map.empty[Long, Long]
        var LRBs: Map[Long, LRB] = Map.empty[Long,LRB]
        var memo: HashMap[(Symbol, Long), Memo] = new HashMap[(Symbol, Long),Memo]

        def get_result(): (List[State], PExp) = {
            (states, exp)
        }

        def save_top(): ParserContext = {
            if(!states.head.trees.isEmpty) forest.top = states.head.trees.head
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
            if(bytesEq(bytes, state.pos, bytes.length)){
                List(state.newLeaf((bytes.map(_.toChar)).mkString).newPos(bytes.length))
            }else{
                forest.cut_forest(state.id, state.lrb)
                List()
            }
        }

        def bytesEq(bytes: Array[Byte], pos: Int, length: Int): Boolean = {
            if((length + pos) > input.length) false else bytes.sameElements(input.slice(pos, pos + length))
        }

        def lookUpRules(symbol: Symbol): PExp = {
            rules(symbol)
        }

        def save_node(symbol: Symbol, state: State): ParserContext = {
            if(states.isEmpty)set_states(List(state)) else set_states(List(state.newNode(symbol, states.head.trees)))
            this
        }

        def add_either(new_state: State, old_state: State): List[State] = {
            List(old_state.addList(new_state.trees))
        }

        def add_both(lhs_state: State, rhs_state: State, old_state: State): List[State] = {
            forest.register(old_state, lhs_state.id, lhs_state.lrb)
            lhs_state:::rhs_state
        }

    }
    

    case class State(var pos: Int, var trees: List[Tree], id: Long, lrb: LRB){
        def new_state(): State = {
            State(pos, List(), id, lrb)
        }

        def newLeaf(v: String): State = {
            trees = trees:+Leaf(v)
            this
        }

        def newNode(name: Symbol, next: List[Tree]): State = {
            trees = trees:+Node(name, next)
            this
        }

        def addList(add: List[Tree]): State = {
            trees = trees:::add
            this
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