
abstract class MyBoolean {
  def ifThenElse[T](t: => T, e: => T): T

  def && (x: => MyBoolean): MyBoolean = ifThenElse(x, False)

  def || (x: => MyBoolean): MyBoolean = ifThenElse(True, x)

  def unary_! : MyBoolean = ifThenElse(False, True)

  def == (x: => MyBoolean): MyBoolean = ifThenElse(x, x.unary_!)

  def != (x: => MyBoolean): MyBoolean = ifThenElse(x.unary_!, x)

  def < (x: => MyBoolean): MyBoolean = ifThenElse(False, x)

  def > (x: => MyBoolean): MyBoolean = ifThenElse(x.unary_!, False)
}

object True extends MyBoolean {
  override def ifThenElse[Boolean](t: => Boolean, e: => Boolean) = t

  override def toString: String = "true"
}

object False extends MyBoolean {
  override def ifThenElse[Boolean](t: => Boolean, e: => Boolean) = e

  override def toString: String = "false"
}

False < True
True < False

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero = true
  override def predecessor: Nothing = throw new NoSuchElementException
  override def successor = new Succ(this)
  override def +(that: Nat) = that
  override def -(that: Nat) = if (that.isZero) this else throw new NoSuchElementException
  override def toString: String = "0"
}

class Succ(n: Nat) extends Nat {
  override def isZero = false
  override def predecessor = n
  override def successor = new Succ(this)
  override def +(that: Nat) = {
    if (that.isZero) this
    else new Succ(this) + that.predecessor
  }
  override def -(that: Nat) = {
    if (that.isZero) this
    else this.predecessor - that.predecessor
  }
  override def toString: String = {
    def loop(nat: Nat, cnt: Int): Int =
      if (nat.isZero) cnt
      else loop(nat.predecessor, cnt + 1)
    loop(this, 0).toString
  }
}

val one = new Succ(Zero)
val two = new Succ(new Succ(Zero))
val three = two + one
val ten = three + two + two + three + Zero
val seven = ten - three
val error = seven - ten