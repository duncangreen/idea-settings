abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}

class Empty extends IntSet {
  def incl(x: Int): IntSet = new NonEmptySet(x, new Empty, new Empty)
  def contains(x: Int): Boolean = false
}

class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def incl(x: Int): IntSet =
    if (x < elem) new NonEmptySet(x, left incl x, right)
    else if (x > elem) new NonEmptySet(x, left, right incl x)
    else this

  override def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
}