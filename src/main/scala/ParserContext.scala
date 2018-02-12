import AST._
import Tree._
import scala.collection.mutable.{HashMap, Set}
import scala.collection.immutable.{Vector}

object ParserContext {
    class ParserContext(_exp: PExp, _rules: Map[Symbol, PExp], _input: Array[Byte]){
        var result: ParserResult = ParserResult(Set(0), Array.fill(_input.length + 1)(Vector.empty[Tree]))
        var exp: PExp = _exp
        private[this] var folding: Boolean = false
        val rules: Map[Symbol, PExp] = _rules
        private[this] val input: Array[Byte] = _input
        private[this] val input_length: Int = _input.length
        private[this] var memo: HashMap[(Symbol, Int), ParserResult] = new HashMap[(Symbol, Int),ParserResult]
        private[this] var bench: Array[Long] = Array(0, 0, 0, 0, 0, 0)

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

        def dump_bench(): ParserContext = {
            println("bench1: " + bench(0) + "回 " + bench(1) + "[ms]")
            this
        }

        def new_result(new_positions: Set[Int]): ParserResult = ParserResult(new_positions, Array.fill(_input.length + 1)(Vector.empty[Tree]))

        def make_result(pos: Int, prev_trees: Array[Vector[Tree]]): ParserResult = ParserResult(Set(pos), prev_trees)

        def set_exp(e: PExp): ParserContext = {
            exp = e
            this
        }

        def set_result(r: ParserResult): ParserContext = {
            result = r
            this
        }

        def map_pos(bytes: Array[Byte]): ParserContext = {
            val new_trees = Array.fill(_input.length + 1)(Vector.empty[Tree])
            result.positions = result.positions.flatMap(pos => match_bytes(bytes, pos, new_trees))
            result.trees = new_trees
            this
        }

        def match_bytes(bytes: Array[Byte], pos: Int, new_trees: Array[Vector[Tree]]): Set[Int] 
        = if(bytesEq(bytes, pos, bytes.length)) result.newLeaf(pos, bytes.length, (bytes.map(_.toChar)).mkString, new_trees) else Set()

        def bytesEq(bytes: Array[Byte], pos: Int, length: Int): Boolean 
        = if((length + pos) > input_length) false else bytes.sameElements(input.slice(pos, pos + length))

        def makeAmbNode(name: Symbol): Node = {
            result.positions.size match{
                case 0 => Node(Symbol("fail"), Vector())
                case 1 => Node(name, result.getHead)
                case _ => Node(name, result.makeAmb)
            }
        }

        def lookup(symbol: Symbol, pos: Int): Option[ParserResult] = memo.get((symbol, pos))

        def memo(symbol: Symbol, pos: Int): ParserResult = {
            memo += ((symbol, pos) -> result.copy())
            this.result
        }

        def newNode(symbol: Symbol): ParserContext = {
            this.set_result(result.newNode(symbol))
        }

        def bench_merge(lhs_result: ParserResult, rhs_result: ParserResult): ParserResult = {
            bench(0)  = bench(0) + 1
            val start = System.currentTimeMillis
            this.set_result(lhs_result.merge(rhs_result))
            val time = System.currentTimeMillis - start
            bench(1) = bench(1) + time
            this.result
        }

        def merge(lhs_result: ParserResult, rhs_result: ParserResult): ParserResult = {
            //this.set_result(lhs_result.merge(rhs_result)).result
            bench_merge(lhs_result, rhs_result)
        }
    }

    case class ParserResult(var positions: Set[Int], var trees: Array[Vector[Tree]]){
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
        
        val update: (Int, Array[Vector[Tree]]) => ParserResult = 
        (pos: Int, prev: Array[Vector[Tree]]) => {
            lazy val prev_tree = prev(pos)
            positions.foreach(i => trees(i) = prev_tree ++ trees(i) )
            this
        }

        def newNode(symbol: Symbol): ParserResult = {
            positions.foreach(pos => trees(pos) = Vector(Node(symbol, trees(pos))))
            this
        }

        val merge: ParserResult => ParserResult = 
        (another: ParserResult) => {
            another.positions.foreach(pos => setTree(pos, another.trees(pos)))
            positions = positions | another.positions
            this
        }
        
        private[this] val setTree: (Int, Vector[Tree]) => Unit =
        (pos: Int, tree: Vector[Tree]) => {
            if(trees(pos).isEmpty) trees(pos) = tree else {
                trees(pos).head match{
                    case an: AmbNode => trees(pos) = trees(pos):+AmbNode(pos, tree)
                    case _ => trees(pos) = Vector(AmbNode(pos, trees(pos)), AmbNode(pos, tree))
                }
            }
        }
        
        def getHead(): Vector[Tree] = trees(positions.head)

        def makeAmb(): Vector[Tree] = {
            var ab = Vector.empty[Tree]
            for(pos <- positions){
                ab = ab:+AmbNode(pos, trees(pos))
            }
            ab
        }
        def newLeaf(pos: Int, len: Int, v: String, new_trees: Array[Vector[Tree]]): Set[Int] = {
            val new_pos = pos + len
            new_trees(new_pos) = new_trees(new_pos):+Leaf(v)
            Set(new_pos)
        }

    }
}