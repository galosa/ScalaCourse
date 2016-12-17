import java.beans.Expression

object T {

  trait Expr {
    def eval: Int = this match {
      case Number(n) => n
      case Sum(e1, e2) => e1.eval + e2.eval
    }

    def show: String = this match {
      case Number(n) => n.toString
      case(Prod(s @ Sum(_,_), e1)) => "(" + s.show + ")" + " * " + e1.show
      case Sum(e1, e2) => e1.show + " + " + e2.show
      case Prod(e1, e2) => e1.show + " * " + e2.show
      case Var(v) => v
    }
  }

  case class Sum(x: Expr, y: Expr) extends Expr

  case class Number(n: Int) extends Expr

  case class Prod(x: Expr, y: Expr) extends Expr

  case class Var(v: String) extends Expr

  val x = Sum(Number(2), Number(3))
  x.show
  x.eval
  Sum(Prod(Number(2), Var("x")), Var("y")).show
  Prod(Sum(Number(2), Var("x")), Var("y")).show
}
