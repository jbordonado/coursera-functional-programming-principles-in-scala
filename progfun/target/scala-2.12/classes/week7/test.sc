import week7.Pouring

object test {
  val problem = new Pouring(Vector(4, 7))
  problem.moves
  problem.pathSets.take(3).toList

  problem.solutions(6)
}