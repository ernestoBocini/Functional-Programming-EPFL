type Glass = Int
type State = Vector[Int]

class Pouring(full: State):

/*let's define the moves */
    enum Move:
        case Empty(glass:Glass)
        case Fill(glass:Glass)
        case Pour(from:Glass, to:Glass)

        def apply(state: State): State =  this match 
            case Empty(glass) => state.updated(glass, 0)
            case Fill(glass) => state.updated(glass, full(glass))
            case Pour(from: Glass, to:Glass) =>
                val amount = state(from) min (full(to) - state(to))
                state.updated(from, state(from) - amount).updated(to, state(to) + amount)

    end Move

/* The next step is what moves do we have? Can we enumerate all the moves? To 
enumerate all the moves let's first enumerate all the glasses which is the range from 0 to 
full.length*/

    val moves = 
        val glasses:Range = 0 until full.length
        (for g<- glasses yield Move.Empty(g))
        ++ (for g<-glasses yield Move.Fill(g))
        ++ (for g1 <- glasses; g2 <- glasses if g1 != g2 yield Move.Pour(g1,g2))

/* next step to model is creating a Path. We want to record paths in reverse order 
So a path is a list of moves, but what we want is the history, so the last move in the path
should come first and the first move from the initial empty state should come last. 
It's also useful to encode what the endstate of the path would be*/

    class Path(history: List[Move], val endState: State):
        def extend(move:Move) = Path(move :: history, move(endState))
        override def toString =  s"${history.reverse.mkString(" ")} --> $endState"
    end Path 

/*now we start from the empty state, which is the full state where we map all te content to 0*/
    val empty: State = full.map(x=> 0)
    val start = Path(Nil, empty)

/* we are now starting the algorithm itself. There are two parts: one construct the infinite
list of possible paths, the other one select the path which brings to the correct solution*/

    def pathsFrom(paths: List[Path], explored: Set[State]): LazyList[List[Path]]=
        val frontier = 
            for
                path <- paths
                move <- moves
                next = path.extend(move)
                if !explored.contains(next.endState)
            yield next
        paths #:: pathsFrom(frontier, explored ++ frontier.map(_.endState))

/*Pick all the paths that lead to the desired state (ie a glass with target unit in it)*/
    def solutions(target: Int): LazyList[Path] =
        for
            paths <- pathsFrom(List(start), Set(empty))
            path <- paths
            if path.endState.contains(target)
        yield path

end Pouring

/*let's try out our algorithm with glasses of sizes 4 and 7 and target 6*/
val problem = Pouring(Vector(4,7))
problem.solutions(6)
/*we want te shortest*/
problem.solutions(6).head


