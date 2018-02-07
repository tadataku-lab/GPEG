import ParserContext._

object Forest{
    sealed trait Tree
    case class Leaf(v: String) extends Tree{
        override def toString: String = {
            "[" + v + "]"
        }
    }
    case class Node(name: Symbol, next: List[Tree]) extends Tree
    case class AmbTree() extends Tree
    case class AmbNode(lhs: Tree, rhs: Tree) extends Tree

    class Forest(){
        var table: Map[Long, Tree] = Map.empty[Long, Tree]
        var jump_table: Map[Long, Long] = Map.empty[Long, Long]
        var top: Tree = null
        override def toString: String = {
            def to_string : (Tree, Long) => String = (tree, id) => {
                tree match {
                    case leaf: Leaf => leaf.toString
                    case Node(name, next) => {
                        val sb = new StringBuilder
                        sb.append("[" + name + " ")
                        for (tree <- next){
                            sb.append(to_string(tree, id))
                        }
                        sb.append("]")
                        sb.toString
                    }
                    case AmbTree() => {
                        val new_id = jump_table(id)
                        table.get(new_id) match {
                            case Some(tree) => to_string(tree, new_id)
                            case None => ""
                        }
                    }
                    case AmbNode(lhs, rhs) => {
                        val sb = new StringBuilder
                        sb.append("[amb<" + id + "> [lhs ")
                        sb.append(to_string(lhs, id))
                        sb.append( "[rhs " + to_string(rhs, id))
                        sb.append("]]")
                        sb.toString
                    }
                }
            }
            to_string(top, 0)
        }

        def cut_forest(id: Long, lrb: LRB): Unit = {
            table.get(id) match{
                case Some(tree) => tree match{
                    case AmbNode(_id, lhs, rhs) => lrb match{
                        case Left() => table = table.updated(id, rhs)
                        case Right() => table = table.updated(id, lhs)
                        case Both() => table = table - id
                    }
                    case _ => table = table - id
                }
                case None => //do nothing
            }
        }

        def save(state: State): Forest = {
            table = table + (state.id -> state.trees.head)
            this
        }

        def register(state: State, id: Long, lrb: LRB): Unit = {
            lrb match {
                case Both() => table = table + (state.id -> state.newAmbTree())
                case Right() => table = table + (state.id -> state.newAmbTree().newAmbNode(false, ))
            }
            table = table + (state.id -> state.newAmbTree())
            jump_table = jump_table + (state.id -> id)
        }
    }
}