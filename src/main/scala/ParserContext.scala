import AST._
import Tree._
import scala.collection.mutable.{ArrayBuffer, HashMap, Set}

object ParserContext {
    class ParserContext(_exp: PExp, _rules: Map[Symbol, PExp], _input: Array[Byte]){
        var result: ParserResult = ParserResult(Set(0), Array.fill(_input.length + 1)(null))
        var exp: PExp = _exp
        private[this] var folding: Boolean = false
        val rules: Map[Symbol, PExp] = _rules
        private[this] val input: Array[Byte] = _input
        private[this] val input_length: Int = _input.length
        private[this] val memos: Array[HashMap[Symbol, Memo]] = Array.fill(_input.length + 1)(HashMap.empty[Symbol, Memo])

        def dump_memo(): ParserContext = {
            println("memo: " + memos)
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

        def new_result(new_positions: Set[Int]): ParserResult = ParserResult(new_positions, Array.fill(_input.length + 1)(null))

        def make_result(pos: Int, prev_trees: Array[ArrayBuffer[Tree]]): ParserResult = ParserResult(Set(pos), prev_trees)

        def set_exp(e: PExp): ParserContext = {
            exp = e
            this
        }

        def set_result(r: ParserResult): ParserContext = {
            result = r
            this
        }

        def map_pos(bytes: Array[Byte]): ParserContext = {
            val new_trees: Array[ArrayBuffer[Tree]] = Array.fill(input.length + 1)(null)
            result.positions = result.positions.flatMap(pos => match_bytes(bytes, pos, new_trees))
            result.trees = new_trees
            this
        }

        def match_bytes(bytes: Array[Byte], pos: Int, new_trees: Array[ArrayBuffer[Tree]]): Set[Int] 
        = if(bytesEq(bytes, pos, bytes.length)) result.newLeaf(pos, bytes.length, (bytes.map(_.toChar)).mkString, new_trees) else Set()

        def bytesEq(bytes: Array[Byte], pos: Int, length: Int): Boolean 
        = if((length + pos) > input_length) false else bytes.sameElements(input.slice(pos, pos + length))

        def makeAmbNode(name: Symbol): Node = {
            result.positions.size match{
                case 0 => Node(Symbol("fail"), null)
                case 1 => Node(name, result.getHead)
                case _ => Node(name, result.makeAmb)
            }
        }

        def lookup(symbol: Symbol, pos: Int): Option[Memo] = memos(pos).get(symbol)

        def memo(symbol: Symbol, pos: Int): Memo = {
            //if(!result.positions.nonEmpty) memo += ((symbol, pos) -> None) else memo += ((symbol, pos) -> Some(result.copy()))
            val memo:Memo = if(result.positions.nonEmpty) Memo(result.positions.clone, newNode(symbol), true) else Memo(Set(), Array(), false)
            memos(pos) += (symbol -> memo)
            memo
        }

        def newNode(symbol: Symbol): Array[Node] = {
            result.newNode(symbol, input_length + 1)
        }
    }

    case class Memo(positions: Set[Int], nodes: Array[Node], isSucc: Boolean){
        val copy: () => Memo = () => Memo(positions.clone, nodes.clone, isSucc)
        val update: ArrayBuffer[Tree] => ParserResult = 
        (prev: ArrayBuffer[Tree]) => {
            if(isSucc){
                val trees:Array[ArrayBuffer[Tree]] = Array.fill(nodes.size)(null)
                if(prev == null){
                    positions.foreach(pos => trees(pos) = ArrayBuffer(nodes(pos)))
                }else{
                    positions.foreach(pos => trees(pos) = prev:+nodes(pos))
                }
                ParserResult(positions, trees)
            }else{
                ParserResult(Set(), null)
            }
        }
    }

    case class ParserResult(var positions: Set[Int], var trees: Array[ArrayBuffer[Tree]]){
        override def toString(): String = {
            if(positions.isEmpty){
                return "{fail}"
            }else{
                var sb = new StringBuilder()
                positions.foreach(pos => sb = sb.append("{ pos<" + pos + "> tree: " + trees(pos) + "}"))
                sb.toString
            }
        }
        def dump(): ParserResult = {
            println(this)
            this
        }
        val copy: () => ParserResult = () => ParserResult(positions.clone, trees.clone)

        def newNode(symbol: Symbol, size: Int): Array[Node] = {
            val nodes:Array[Node] = Array.fill(size)(null)
            positions.foreach(pos => nodes(pos) = Node(symbol, if(trees(pos) == null) Array() else trees(pos).toArray.clone))
            nodes
        }

        val merge: ParserResult => ParserResult = 
        (another: ParserResult) => {
            another.positions.foreach(pos => setTree(pos, another.trees(pos)))
            positions = positions | another.positions
            this
        }
        
        private[this] val setTree: (Int, ArrayBuffer[Tree]) => Unit =
        (pos: Int, tree: ArrayBuffer[Tree]) => {
            if(trees(pos) == null) trees(pos) = tree else {
                trees(pos).head match{
                    case an: AmbNode => trees(pos) = trees(pos):+AmbNode(pos, tree.toArray)
                    case _ => trees(pos) = ArrayBuffer(AmbNode(pos, trees(pos).toArray), AmbNode(pos, tree.toArray))
                }
            }
        }
        
        def getHead(): Array[Tree] = trees(positions.head).toArray

        def makeAmb(): Array[Tree] = {
            var ab = ArrayBuffer.empty[Tree]
            for(pos <- positions){
                ab = ab:+AmbNode(pos, trees(pos).toArray)
            }
            ab.toArray
        }
        def newLeaf(pos: Int, len: Int, v: String, new_trees: Array[ArrayBuffer[Tree]]): Set[Int] = {
            val new_pos = pos + len
            trees(pos) match{
                case null => new_trees(new_pos) = ArrayBuffer(Leaf(v))
                case _ => new_trees(new_pos) = trees(pos):+Leaf(v)
            }
            Set(new_pos)
        }

    }
}