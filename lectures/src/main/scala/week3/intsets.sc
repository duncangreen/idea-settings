abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(that: IntSet) : IntSet
}

object Empty extends IntSet {
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def contains(x: Int): Boolean = false
  override def toString: String = "."
  override def union(that: IntSet): IntSet = that
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

  override def toString: String = "{" + left + elem + right + "}"

  override def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem
}

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4
val t3 = t2 union t1

Empty contains 1

new NonEmpty(7, Empty, Empty) contains 7
