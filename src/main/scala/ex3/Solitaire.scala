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

  type Solution = Iterable[Int]
  type IterableFactory = Solution => Iterable[Solution]
  given IterableFactory = LazyList(_)

  /*def placeMarks(boardSize: Int): Iterable[Solution] =
    for
      x <- 0 to boardSize
      y <- 0 to boardSize
      numberPos = (x, y)*/

  println(render(solution = Seq((0, 0), (2, 1)), width = 3, height = 3))