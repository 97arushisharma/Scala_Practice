package Course2

class WaterPouring( capacity : Vector[Int]) {
  // States
  type State = Vector[Int]
  val initialState = capacity map (x => 0)

  // Moves
  trait Move{
    def change(state: State) : State
  }
  case class Empty( glass: Int) extends Move {
    def change(state: State) : State = state updated(glass,0)
  }
  case class Fill( glass: Int) extends Move {
    def change(state: State) : State = state updated(glass, capacity(glass))
  }
  case class Pour( from: Int, to: Int) extends Move {
    def change(state: State) : State = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated (from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  val glasses = 0 until capacity.length

  val moves = (for(g <- glasses) yield Empty(g)) ++ (for(g <- glasses) yield Fill(g)) ++
    (for(from <- glasses; to <- glasses if from != to) yield Pour(from, to))

  // Paths in Stack format
  //    trackState(history)
  //    private def trackState(xs : List[Move]): State = xs match {
  //        case Nil => initialState
  //        case move :: xs1 => move change trackState(xs1)
  //    }

  class Path( history : List[Move], val endState: State) {
    // as paths get longer, endStates gets heavy to recompute and val because it should be available from outside
    // def endState : State = (history foldRight initialState) ( _ change _ )
    def extend(move : Move) = new Path(move :: history, move change endState)
    override def toString: String = (history.reverse mkString " ") + "-->" + endState
  }

  val initialPath = new Path(Nil, initialState)

  def from(paths : Set[Path], explored: Set[State]) : Stream[Set[Path]] = {
    if(paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }
  }

  // This will give the set of all paths of length 1 then 2, then 3 ..... from the initialPath
  val pathSets = from(Set(initialPath), Set(initialState))

  def solution(target: Int): Stream[Path] = {
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
  }

}
