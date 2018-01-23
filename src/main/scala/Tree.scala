
object Tree{
    sealed trait Tree
    case class Leaf(v: String) extends Tree{
        override def toString: String = {
            "[" + v + "]"
        }
    }
    case class Node(name: Symbol, next: List[Tree]) extends Tree{
        override def toString: String = {
            val sb = new StringBuilder
            sb.append("[" + name + " ")
            for (tree <- next){
                sb.append(tree)
            }
            sb.append("]")
            sb.toString
        }
    }
    case class AmbNode(id: Int, lhs: List[Tree], rhs: List[Tree]) extends Tree{
        override def toString: String = {
            val sb = new StringBuilder
            sb.append("[amb<" +id + "> ")
            sb.append("[lhs ")
            for (_lhs <- lhs){
                sb.append(_lhs)
            }
            sb.append("][rhs ")
            for (_rhs <- rhs){
                sb.append(_rhs)
            }
            sb.append("]]")
            sb.toString
        }
    }
}