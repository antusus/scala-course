package week3

object Empty extends IntSet {
    override def toString: String = "."
    override def contains(x: Int): Boolean = false
    override def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
    override def union(other: IntSet): IntSet = other
}
