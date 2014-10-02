import week3._

object intsets {
    val t1 = new NonEmpty(8, Empty, Empty)
    val t2 = new NonEmpty(4, Empty, Empty)
    val t3 = new NonEmpty(2, Empty, Empty)
    (t1 union t2) union t3
}

