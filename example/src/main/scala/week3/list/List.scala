package week3.list

trait List[T] {
    def isEmpty: Boolean

    def head: T

    def tail: List[T]
}
