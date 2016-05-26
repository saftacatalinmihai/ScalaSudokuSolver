
import rx.lang.scala.Observable
import rx.lang.scala.schedulers.ComputationScheduler

import scala.collection.immutable.HashMap
import rx.lang.scala.Scheduler
import scala.concurrent.duration.DurationInt
/**
  * Created by casafta on 16/5/2016.
  */

abstract class Puzzle {

  def cells: Map[Loc, Cell]
  def isSolved: Boolean
  def isImpossible: Boolean
  def setCell(loc: Loc, v: Value): Puzzle =
    cells(loc) match {
      case c: KnownCell => if (c.value == v) this else new ImpossiblePuzzle(cells)
      case c: ImpossibleCell => this
      case _ =>
        val setCellPuzzle: Puzzle = Puzzle(cells + (loc -> Cell(v)))
        Puzzle.dependentLocs(loc).foldLeft(setCellPuzzle)(
          (pAcc, l) => pAcc match {
            case p@(_: ImpossiblePuzzle | _: SolvedPuzzle) => p
            case _ => pAcc.cells(l) match {
              case c: KnownCell => if (c.value == v) new ImpossiblePuzzle(pAcc.cells) else pAcc
              case c: ImpossibleCell => new ImpossiblePuzzle(setCellPuzzle.cells)
              case ce: EmptyCell => pAcc.removePossibleValFromCell(l, v)
              case cwv: CellWithVals => pAcc.removePossibleValFromCell(l, v).cells(l) match {
                case c: KnownCell => pAcc.setCell(l, c.value)
                case c: ImpossibleCell => new ImpossiblePuzzle(pAcc.cells)
                case _ => pAcc.removePossibleValFromCell(l, v)
              }
            }
          }
        )
    }

  def removePossibleValFromCell(l: Loc, v: Value): Puzzle =
    Puzzle(cells + (l -> cells(l).removePossibleVal(v)))

  def knownCells = cells.filter( _._2.isKnown )

  override def toString = {
    val strList = for {
      i <- 1 to 9
      j <- 1 to 9
    } yield {
      val str = cells(Loc(i,j)) match {
        case KnownCell(v: Value) =>
          v match {
            case KnownValue(intVal) => "[        " + intVal + "         ] "
            case _ => "[        ?         ] "
          }
        case EmptyCell() => "[* * * * * * * * * ] "
        case CellWithVals(vals) =>
          "[" +
            (1 to 9).map(
                        value => if (vals.contains(Value(value))) value.toString else "*"
                      ).foldRight("")( (s1, s2) => s1 + " " + s2) +
            "] "
        case _ => "[        ?         ] "
      }
      if (j == 9) str + "\n" else str
    }
    "\n"+strList.foldLeft("")( (s1, s2) => s1 + s2)
  }
}

case class UnsolvedPuzzle(cells: Map[Loc, Cell]) extends Puzzle{
  def isSolved = false
  def isImpossible = false
}

case class SolvedPuzzle(val cells: Map[Loc, Cell] ) extends Puzzle{
  override def isSolved: Boolean = true
  def isImpossible = false
  override def removePossibleValFromCell(l: Loc, v: Value): Puzzle = this
  override def setCell(l: Loc, v: Value): Puzzle = this
  override def toString = {
    val headerAndFooter = "+-----------------------+\n"
    val midSection =  "|-------+-------+-------|\n"
    val str =
      for {
      i <- 1 to 9
      j <- 1 to 9
    } yield {
      val str =
        {
          if (j == 1 && (i == 4 || i == 7))
            midSection
          else
            ""
        } +
        {
          if (j == 1 )
            "|"
          else
            ""
        } +
        cells(Loc(i, j)) +
        {
          if (j == 3 || j == 6 )
            " |"
          else if ( j == 9)
            " |\n"
          else
            ""
        }
      str
    }
    "\n" +
    headerAndFooter +
      str.foldLeft("")(
          (s1, s2) => s1 + "" + s2
      ) +
    headerAndFooter
  }
}

case class ImpossiblePuzzle(val cells: Map[Loc, Cell]) extends Puzzle{
  override def isSolved: Boolean = false
  def isImpossible = true
  override def removePossibleValFromCell(l: Loc, v: Value): Puzzle = this
  override def setCell(l: Loc, v: Value): Puzzle = this
}

case class Pair(l: Loc, c: Cell)

object Puzzle {

  def which(p: Puzzle): String =
    p match {
      case ImpossiblePuzzle(_) => "Impossible"
      case UnsolvedPuzzle(_) => "Unsolved"
      case SolvedPuzzle(_) => "Solved"
    }

  def isPossible(cells: Map[Loc, Cell]): Boolean = {
    cells.forall(
      cellMap => {
        val (loc, cell) = (cellMap._1, cellMap._2)
        val depCells = Puzzle.dependentLocs(loc)
        depCells.filter(l => cells(l).isKnown).forall(
          depLoc => cells(depLoc).value != cell.value
        )
      }
    )
  }

  type CellList = List[Map[Loc, Cell]]
  def areAllCellsKnown(p: Puzzle): Boolean =
    p.cells.forall {
      (m) => {
        val (_, c) = (m._1, m._2)
        c.isKnown
      }
    }

  def solve(p: Puzzle)(implicit scheduler: Scheduler = ComputationScheduler()): Observable[Puzzle] =
    p match {
      case p: SolvedPuzzle => Observable.just(p)
      case p: ImpossiblePuzzle => Observable.empty
      case unsolvedPuzzle: UnsolvedPuzzle =>

        Observable
          .from(Puzzle.cellsByLeastPossibleVals(unsolvedPuzzle).head )
          .flatMap( lc => {
            val (loc, cell) = (lc._1, lc._2)
            Observable.from(cell.possibleVals.map(
              pv => unsolvedPuzzle.setCell(loc, pv)
            ))
          })
          .subscribeOn( scheduler )
          .flatMap(Puzzle.solve)
    }

  def cellsByLeastPossibleVals(p: Puzzle): CellList =
    p.cells
      .filter( k => !p.cells(k._1).isKnown)
      .keys.map(
        loc => HashMap(loc -> p.cells(loc))
        )
      .toList
      .sortBy(
        m => m.values.head.possibleVals.size
        )

  var depLocCache: Map[Loc, List[Loc]] = Map()
  def dependentLocs(l: Loc): List[Loc] = {
    depLocCache.get(l) match {
      case Some(list) =>  list
      case _ =>
        val (row, col) = (l.i, l.j)
        val rowLocs = (1 to 9).filter(_ != row).map(Loc(_, col))
        val colLocs = (1 to 9).filter(_ != col).map(Loc(row, _))
        val blockLocs = {
          val b_row = (row - 1) / 3
          val b_col = (col - 1) / 3
          for {
            i <- (3 * b_row + 1) until (3 * b_row + 4)
            j <- (3 * b_col + 1) until (3 * b_col + 4)
          } yield Loc(i, j)
        }
        depLocCache = depLocCache + (
            l -> (rowLocs ++ colLocs ++ blockLocs).distinct.filter(_!=l).toList
          )
        depLocCache(l)
    }

  }

  def apply(cells: Map[Loc, Cell]): Puzzle =  {
    if (cells.values.forall(_.isKnown)) new SolvedPuzzle(cells)
    else if (cells.values.exists(_.isImpossible)) new ImpossiblePuzzle(cells)
    else if (!Puzzle.isPossible(cells)) new ImpossiblePuzzle(cells)
    else new UnsolvedPuzzle(cells)
  }

  def apply(): Puzzle = {
    new UnsolvedPuzzle(
      (1 to 9).flatMap(
        row => {
          (1 to 9).map(
            (col) => (row, col)
          )
        }
      ).foldLeft(Map[Loc, Cell]())(
        (m, t) => m + (Loc(t._1, t._2) -> Cell())
      ))
  }
}

object Test{
  def main(args: Array[String]) {
//    val p = Puzzle()
//    val p = FilePuzzleIO.read("test.puzzle")
    val p = FilePuzzleIO.read("hardest.puzzle")

    println("Start")
    val t0 = System.nanoTime()

    Puzzle.solve(p).head.subscribe((p) => {
      val t1 = System.nanoTime()
      println(p)
      println("Elapsed time: " + (t1 - t0) + " ns")
    })

    scala.io.StdIn.readLine()
  }
}