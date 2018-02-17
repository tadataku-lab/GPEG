
object Tree{
    sealed trait Tree{
        def copy(): Tree
    }

    case class ChildTree(tree: Tree, log: ChildTree){
        override def toString: String = {
            val sb = new StringBuilder
            if(log != null) sb.append(log.toString)
            sb.append(tree.toString)
            sb.toString 
        }
        def copy(): ChildTree = ChildTree(tree.copy, log.copy)
    }

    case class Leaf(v: String) extends Tree{
        override def toString: String = {
            "[" + v + "]"
        }
        def copy(): Leaf = Leaf(v)
    }
    case class Node(name: Symbol, next: ChildTree) extends Tree{
        override def toString: String = {
            val sb = new StringBuilder
            sb.append("[" + name + " ")
            sb.append(next.toString)
            sb.append("]")
            sb.toString
        }
        def copy(): Node =  Node(name, next.copy)
    }
    case class AmbNode(id: Int, next: ChildTree) extends Tree{
        override def toString: String = {
            val sb = new StringBuilder
            sb.append("[amb<" + id + "> ")
            sb.append(next.toString)
            sb.append("]")
            sb.toString
        }
        def copy(): AmbNode = AmbNode(id, next.copy)
    }
}