package week3.list

import week3.list

class Cons[T](val head: T, val tail: list.List[T]) extends List[T] {
    override def isEmpty: Boolean = false
}

