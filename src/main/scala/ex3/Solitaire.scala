package ex3

object Solitaire extends App:
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  type Pos = (Int, Int)
  type Solution = Iterable[Pos]
  type IterableFactory = Solution => Iterable[Solution]
  given IterableFactory = LazyList(_)

  private val MINIMUM_BOARD_SIZE = 5

  def placeMarks(boardSize: Int)(using factory: IterableFactory): Iterable[Solution] =
    def placeMarksHelper(solutionUntilNow: Solution): Iterable[Solution] = boardSize match
      case n if n < MINIMUM_BOARD_SIZE || solutionUntilNow.size == boardSize * boardSize => factory(solutionUntilNow)
      case _ =>
        for
          x <- 0 until boardSize
          y <- 0 until boardSize
          numberPos = (x, y)
          if isPositionFree(numberPos, solutionUntilNow)
          if isValidPosition(numberPos, solutionUntilNow)
          solution <- placeMarksHelper(numberPos +: solutionUntilNow.toSeq) //placeholder
        yield
          solution

    placeMarksHelper(List((boardSize / 2, boardSize / 2)))

  private def isValidPosition(position: (Int, Int), others: Iterable[Pos]): Boolean = if others.isEmpty then true
     else
      others.head match
        case (prevRow, prevCol) =>
          val rowDiff = math.abs(position._1 - prevRow)
          val colDiff = math.abs(position._2 - prevCol)
          (rowDiff == 3 && colDiff == 0) || (rowDiff == 0 && colDiff == 3) || (rowDiff == 2 && colDiff == 2)


  private def isPositionFree(position: (Int, Int), others: Iterable[Pos]) = !others.toSeq.contains(position)


  placeMarks(5).foreach(s =>
    println(render(s.toSeq, width = 5, height = 5))
    println("----------")
  )
  //println(render(solution = Seq((0, 0), (2, 1)), width = 3, height = 3))