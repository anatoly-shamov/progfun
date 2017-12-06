abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  override def incl(x: Int) = new NonEmpty(x, Empty, Empty)

  override def contains(x: Int) = false

  override def union(other: IntSet) = other

  override def toString: String = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  override def union(other: IntSet) = left.union(right.union(other.incl(elem)))

  override def toString: String = "{" + left + elem + right + "}"
}

val mySet1 = new NonEmpty(5, Empty, Empty).incl(2).incl(11)
val mySet2 = new NonEmpty(6, Empty, Empty).incl(4).incl(15)

mySet1.union(mySet2)
