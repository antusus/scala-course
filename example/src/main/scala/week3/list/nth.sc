import week3.list._
object nth {
    def nth[T](n: Int, xs: List[T]): T = {
        if(xs.isEmpty) throw new IndexOutOfBoundsException
        else if (n == 0) xs.head
        else nth(n - 1, xs.tail)
    }
    var list: Cons[Int] = new Cons(1, new Cons(2, new Cons(3, new Nil)))
    nth(0, list)
    nth(2, list)
//    nth(-1, list)
//    nth(4, list)
}