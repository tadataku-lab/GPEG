import ContextTree._
import Tree._

object Memo{
    class Memo(_context: ContextTree, _tree: List[Tree]){
        val context = _context
        val tree = _tree
        override def toString: String = {
            val sb = new StringBuilder
            sb.append("context: " + context + " tree: " + tree.toString)
            sb.toString
        }
    }
}