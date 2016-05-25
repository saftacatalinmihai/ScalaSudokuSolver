
import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by casafta on 19/5/2016.
  */
class PuzzleSpec extends FlatSpec with Matchers{

  "An empty Puzzle" should "be created" in {
    val p = Puzzle()
    p.cells.values.forall(_.possibleVals.size == 9) should be (true)
    p.cells.values.foreach(
      _.possibleVals should have size 9
    )
    p should be (UnsolvedPuzzle(p.cells))
  }

  it should "set the value of a cell and remove that value from dependent cells" in {
    val p = Puzzle()
    val pSet = p.setCell(Loc(1,1), 1)
    pSet.cells(Loc(1, 1)) should be (KnownCell(1))
    for (
      depCell <- Puzzle.dependentLocs(Loc(1,1)).map(pSet.cells(_))
    ) {
      depCell.possibleVals should be (List(2,3,4,5,6,7,8,9).map(Value(_)).toSet)
    }
  }

  "A nonempty Puzzle" should "be created" in {
    val p = Puzzle()
      .setCell(Loc(1,1), 1)

    p.knownCells.values.toList.length should be (1)
  }

  it should "do nothing if trying to set the same cell again" in {
    val p = Puzzle()
      .setCell(Loc(1,1), 1)

    val samePuzzle = p.setCell(Loc(1,1),1)
    samePuzzle should be (UnsolvedPuzzle(samePuzzle.cells))
    p should be (samePuzzle)
  }

  it should "be set as impossible if trying to set 2 dependent cells to the same value" in {
    val p = Puzzle()

    for {
      i <- 1 to 9
      j <- 1 to 9
      n <- 1 to 9
    } {
      val setPuzzle = p.setCell(Loc(i, j), n)
      for (depLoc <- Puzzle.dependentLocs(Loc(i, j))){
        val impossible = setPuzzle.setCell(depLoc, n)
        impossible should be (ImpossiblePuzzle(impossible.cells))
      }
    }
  }

  "Dependent cells" should "be correct" in {
    for {
      i <- 1 to 9
      j <- 1 to 9
    } {
      val dep = Puzzle.dependentLocs(Loc(i,j))
      dep.forall( l => Puzzle.dependentLocs(l).contains(l))
    }
  }

  "A puzzle" should "be read from file" in {
    val p = FilePuzzleIO.read("test.puzzle")
    p should be (UnsolvedPuzzle(p.cells))
  }

  "Reading a puzzle from file" should
    "correctly set dependent cells by removing know values from the possible values of dependent cells" in {
    val p = FilePuzzleIO.read("test.puzzle")

    p.cells(Loc(1,2)).possibleVals should be (List(1, 3, 5, 6).map(Value(_)).toSet)

    p.cells(Loc(5,4)).possibleVals should be (List(3, 5, 6, 9).map(Value(_)).toSet)

    p.cells(Loc(7,9)).possibleVals should be (List(4,7).map(Value(_)).toSet)
  }

  it should "be impossible to set the value of a dependent cell to that of the cell" in {
    val p = FilePuzzleIO.read("test.puzzle")
    List(
      Loc(1,2),
      Loc(1,3),
      Loc(2,1),
      Loc(2,2),
      Loc(5,1),
      Loc(7,1)
    ).map( p.setCell(_, 4))
     .foreach( p => p should be (ImpossiblePuzzle(p.cells)))

    List(
      Loc(4,5),
      Loc(1,4),
      Loc(3,4),
      Loc(4,7),
      Loc(4,6),
      Loc(5,6),
      Loc(4,7)
    ).map( p.setCell(_, 2))
      .foreach( pn => pn should be (ImpossiblePuzzle(pn.cells)))

    List(
      Loc(7,4),
      Loc(7,5),
      Loc(7,6),
      Loc(8,5),
      Loc(9,4),
      Loc(9,5),
      Loc(9,6),
      Loc(8,1),
      Loc(8,9),
      Loc(1,4),
      Loc(9,4)
    ).map( p.setCell(_, 4))
      .foreach( pn => pn should be (ImpossiblePuzzle(pn.cells)))

  }

}
