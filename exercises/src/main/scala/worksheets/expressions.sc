trait Expr {
  def eval: Int = this match {
    case Num(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
    case Prod(e1, e2) => e1.eval * e2.eval
  }

  def show: String = this match {
    case Num(n) => n.toString
    case Var(x) => x
    case Sum(e1, e2) => e1.show + " + " + e2.show
    case Prod(Sum(e1, e2), Sum(e3, e4)) =>
      "(" + e1.show + " + " + e2.show + ")" + " * " +
        "(" + e3.show + " + " + e4.show + ")"
    case Prod(e1, Sum(e2, e3)) =>
      e1.show + " * " + "(" + e2.show + " + " + e3.show + ")"
    case Prod(Sum(e1, e2), e3) =>
      "(" + e1.show + " + " + e2.show + ")" + " * " + e3.show
    case Prod(e1, e2) => e1.show + " * " + e2.show
  }

  override def toString: String = this.show
}

case class Num(val n: Int) extends Expr

case class Sum(val e1: Expr, val e2: Expr) extends Expr

case class Prod(val e1: Expr, val e2: Expr) extends Expr

case class Var(val x: String) extends Expr

Sum(Prod(Num(2), Var("x")), Var("y"))
Sum(Var("x"), Num(5))
Prod(Var("x"), Num(5))
Prod(Sum(Num(2), Var("x")), Var("y"))
Prod(Var("y"), Sum(Num(2), Var("x")))
Prod(Sum(Prod(Sum(Num(2), Var("x")), Var("y")), Var("x")), Sum(Num(3), Var("y")))
