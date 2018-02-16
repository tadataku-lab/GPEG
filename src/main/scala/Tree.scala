
object Tree{
    sealed trait Tree{
        def copy(): Tree
    }
    case class Leaf(v: String) extends Tree{
        override def toString: String = {
            "[" + v + "]"
        }
        def copy(): Leaf = Leaf(v)
    }
    case class Node(name: Symbol, next: Array[Tree]) extends Tree{
        override def toString: String = {
            val sb = new StringBuilder
            sb.append("[" + name + " ")
            for (tree <- next){
                sb.append(tree.toString)
            }
            sb.append("]")
            sb.toString
        }
        def copy(): Node =  Node(name, next.flatMap(n => Array(n.copy)))
    }
    case class AmbNode(id: Int, next: Array[Tree]) extends Tree{
        override def toString: String = {
            val sb = new StringBuilder
            sb.append("[amb<" + id + "> ")
            for (tree <- next){
                sb.append(tree.toString)
            }
            sb.append("]")
            sb.toString
        }
        def copy(): AmbNode = AmbNode(id, next.flatMap(n => Array(n.copy)))
    }
}