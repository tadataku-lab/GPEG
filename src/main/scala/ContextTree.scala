import AST._
import scala.collection.mutable.{HashMap}
import Memo._

object ContextTree{
    sealed trait ContextTree {
        def setExp(body: PExp): ContextTree = {
            this match {
                case a_c: AmbContext => {
                    a_c.lhs.setExp(body)
                    a_c.rhs.setExp(body)
                    a_c
                }
                case p_c: ParserContext => {
                    p_c.exp = body
                    p_c
                }
            }
        }

        def setFold(folding: Boolean): ContextTree = {
            this match {
                case a_c: AmbContext => {
                    a_c.lhs.setFold(folding)
                    a_c.rhs.setFold(folding)
                    a_c
                }
                case p_c: ParserContext => {
                    p_c.folding = folding
                    p_c
                }
            }
        }

        def copy(): ContextTree
        def toString() : String
    }

    case class AmbContext(_lhs: ContextTree, _rhs: ContextTree, _id: Int) extends ContextTree{
        val lhs = _lhs
        val rhs = _rhs
        val id = _id
        def copy(): AmbContext = {
            new AmbContext(lhs.copy, rhs.copy, id)
        }
        override def toString: String = {
            val sb = new StringBuilder
            sb.append("{[lhs: " + lhs.toString + " ][rhs: " + rhs.toString + " ]}")
            sb.toString
        }
    }

    case class ParserContext(_pos: Int, start: PExp, _hash_table: HashMap[(Symbol, Int),Memo], startN: Symbol, _folding: Boolean) extends ContextTree{
        var pos = _pos
        var exp = start
        var hash_table = _hash_table
        var nonterm = startN
        var folding = _folding

        def copy(): ParserContext = {
            new ParserContext(pos, exp.copy, hash_table, nonterm, folding)
        }

        override def toString: String = {
            val sb = new StringBuilder
            sb.append("{pos<" + pos + ">exp<" + exp.toString + ">}")
            sb.toString
        }
    }
}