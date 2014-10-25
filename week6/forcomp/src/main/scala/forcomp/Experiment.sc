object Experiment {
    val letters = List("a", "b", "c")
    val numbers = List(1, 2)
    letters.zipAll(numbers, "x", 0)
}