import AST._

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

    case class AmbContext(lhs: ContextTree, rhs: ContextTree, id: Int) extends ContextTree{

        def copy(): AmbContext = {
            new AmbContext(lhs.copy, rhs.copy, id)
        }
        override def toString: String = {
            val sb = new StringBuilder
            sb.append("{[lhs: " + lhs.toString + " ][rhs: " + rhs.toString + " ]}")
            sb.toString
        }
    }

    case class ParserContext(var pos: Int, var exp: PExp, var nonterm: Symbol, var folding: Boolean) extends ContextTree{

        def copy(): ParserContext = {
            new ParserContext(pos, exp.copy, nonterm, folding)
        }

        override def toString: String = {
            val sb = new StringBuilder
            sb.append("{pos<" + pos + ">exp<" + exp.toString + ">}")
            sb.toString
        }
    }
}