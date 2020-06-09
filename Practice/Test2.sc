import Course2.WaterPouring

object Test2 {
  val problem = new WaterPouring(Vector(4,7))
  problem.moves
  problem.pathSets.take(3).toList
  problem.solution(17)
}