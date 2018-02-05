import ContextTree._
import Tree._

object Memo{
    case class Memo(v: Option[(List[Tree], ContextTree)]){
    /**
        override def toString: String = {
            val sb = new StringBuilder
            sb.append("context: " + context + " tree: " + tree.toString)
            sb.toString
        }
        */
    }
}