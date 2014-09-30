object intsets {
    val t1 = new NonEmpty(8, Empty, Empty)
    val t2 = new NonEmpty(4, Empty, Empty)
    val t3 = new NonEmpty(2, Empty, Empty)
    (t1 union t2) union t3
}
abstract class IntSet {
    def contains(x: Int): Boolean
    def incl(x: Int): IntSet
    def union(other: IntSet): IntSet
}
object Empty extends IntSet {
    override def toString: String = "."
    override def contains(x: Int): Boolean = false
    override def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
    override def union(other: IntSet): IntSet = other
}
class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    override def toString: String = "{" + left + elem + right + "}"
    override def contains(x: Int): Boolean =
        if (elem < x) left contains x
        else if (elem > x) right contains x
        else true
    override def incl(x: Int): IntSet =
        if (elem > x) new NonEmpty(elem, left incl x, right)
        else if (elem < x) new NonEmpty(elem, left, right incl x)
        else this


    override def union(other: IntSet): IntSet =
        ((left union right) union other) incl elem
}