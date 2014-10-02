package week3

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