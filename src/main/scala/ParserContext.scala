import AST._
import Tree._
import scala.collection.mutable.{BitSet}

object ParserContext {
    class ParserContext(_exp: PExp, _rules: Array[PExp], _symbols: Array[Symbol], _input: Array[Byte]){
        var result: ParserResult = ParserResult(BitSet(0), Array.fill(_input.length + 1)(null))
        var exp: PExp = _exp
        private[this] var folding: Boolean = false
        val rules: Array[PExp] = _rules
        val symbols: Array[Symbol] = _symbols
        private[this] val input: Array[Byte] = _input
        private[this] val input_length: Int = _input.length
        private[this] val memos: Array[Memo] = Array.fill((_input.length + 1) * symbols.length)(Memo(-1, null, null, false))

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

        def new_result(new_positions: BitSet): ParserResult = ParserResult(new_positions, Array.fill(_input.length + 1)(null))

        def make_result(pos: Int, prev_trees: Array[ChildTree]): ParserResult = ParserResult(BitSet(pos), prev_trees)

        def set_exp(e: PExp): ParserContext = {
            exp = e
            this
        }

        def set_result(r: ParserResult): ParserContext = {
            result = r
            this
        }

        def map_pos(bytes: Array[Byte]): ParserContext = {
            val new_trees: Array[ChildTree] = Array.fill(input.length + 1)(null)
            result.positions = result.positions.flatMap(pos => match_bytes(bytes, pos, new_trees))
            result.trees = new_trees
            this
        }

        def match_bytes(bytes: Array[Byte], pos: Int, new_trees: Array[ChildTree]): BitSet 
        = if(bytesEq(bytes, pos, bytes.length)) result.newLeaf(pos, bytes.length, (bytes.map(_.toChar)).mkString, new_trees) else BitSet()

        def bytesEq(bytes: Array[Byte], pos: Int, length: Int): Boolean 
        = if((length + pos) > input_length) false else bytes.sameElements(input.slice(pos, pos + length))

        def makeAmbNode(name: Symbol): Node = {
            result.positions.size match{
                case 0 => Node(Symbol("fail"), null)
                case 1 => Node(name, result.getHead)
                case _ => Node(name, result.makeAmb)
            }
        }

        def longkey(key: Int, mpos: Int): Int ={
            key + mpos * this.input_length
        }

        def lookup(nsym: Int, pos: Int): Memo = memos(longkey(pos, nsym))

        def memo(nsym: Int, pos: Int): Memo = {
            //if(!result.positions.nonEmpty) memo += ((symbol, pos) -> None) else memo += ((symbol, pos) -> Some(result.copy()))
            val memo:Memo = if(result.positions.nonEmpty) Memo(pos, result.positions.clone, newNode(symbols(nsym)), true) else Memo(pos, BitSet(), Array(), false)
            memos(longkey(pos, nsym)) = memo
            memo
        }

        def newNode(symbol: Symbol): Array[Node] = {
            result.newNode(symbol, input_length + 1)
        }
    }

    case class Memo(key: Int, positions: BitSet, nodes: Array[Node], isSucc: Boolean){
        //val copy: () => Memo = () => Memo(key, positions.clone, nodes.clone, isSucc)
        val update: ChildTree => ParserResult = 
        (prev: ChildTree) => {
            if(isSucc){
                val trees:Array[ChildTree] = Array.fill(nodes.size)(null)
                if(prev == null){
                    positions.foreach(pos => trees(pos) = ChildTree(nodes(pos), null))
                }else{
                    positions.foreach(pos => trees(pos) = ChildTree(nodes(pos), prev))
                }
                ParserResult(positions, trees)
            }else{
                ParserResult(BitSet(), null)
            }
        }
    }

    case class ParserResult(var positions: BitSet, var trees: Array[ChildTree]){
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
            positions.foreach(pos => nodes(pos) = Node(symbol, trees(pos)))
            nodes
        }

        val merge: ParserResult => ParserResult = 
        (another: ParserResult) => {
            another.positions.foreach(pos => setTree(pos, another.trees(pos)))
            positions = positions | another.positions
            this
        }
        
        private[this] val setTree: (Int, ChildTree) => Unit =
        (pos: Int, tree: ChildTree) => {
            if(trees(pos) == null) trees(pos) = tree else {
                trees(pos).tree match{
                    case an: AmbNode => trees(pos) = ChildTree(AmbNode(pos, tree), trees(pos))
                    case _ => trees(pos) = ChildTree(AmbNode(pos, tree), ChildTree(AmbNode(pos, trees(pos)), null))
                }
            }
        }
        
        def getHead(): ChildTree = trees(positions.head)

        def makeAmb(): ChildTree = {
            var ab: ChildTree = null
            for(pos <- positions){
                ab = ChildTree(AmbNode(pos, trees(pos)), ab)
            }
            ab
        }
        def newLeaf(pos: Int, len: Int, v: String, new_trees: Array[ChildTree]): BitSet = {
            val new_pos = pos + len
            trees(pos) match{
                case null => new_trees(new_pos) = ChildTree(Leaf(v), null)
                case _ => new_trees(new_pos) = ChildTree(Leaf(v), trees(pos))
            }
            BitSet(new_pos)
        }

    }
}