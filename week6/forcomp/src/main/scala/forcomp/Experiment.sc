object Experiment {
    val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 2))
    val y = List(('r', 1))

    val mapx = x.toMap
    y.foldLeft(mapx)((acc, entry) => {
        val occurance: Int = mapx.apply(entry._1)
        if(occurance == entry._2) acc - entry._1
        else acc.updated(entry._1, occurance - entry._2)
    }).toList.sortBy(touple => touple._1)
}