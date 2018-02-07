import ContextTree._
import Forest._

object Memo{
    case class Memo(v: Option[(Int, Tree)]){
        /**
        override def toString: String = {
            val sb = new StringBuilder
            sb.append("context: " + context + " tree: " + tree.toString)
            sb.toString
        }
        */
    }
}