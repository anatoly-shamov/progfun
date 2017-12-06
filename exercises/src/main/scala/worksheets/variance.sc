trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend [U >: T] (elem: U): List[U] = new Cons(elem, this)
}
class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  override def isEmpty: Boolean = false
}
object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException
  override def tail: Nothing = throw new NoSuchElementException
}

val x: List[String] = Nil

def f(xs: List[Int], x: Boolean) = xs prepend x