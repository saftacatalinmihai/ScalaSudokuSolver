//val p = Puzzle()
//p.cells
//p.setCell(Loc(1,1),1)
//  .setCell(Loc(1,2),2)
//  .setCell(Loc(1,3),3)
//  .setCell(Loc(2,1),4)
//  .setCell(Loc(2,2),5)
//  .setCell(Loc(2,3),6)
//  .setCell(Loc(3,1),7)
//  .setCell(Loc(3,2),8)
//  .setCell(Loc(4,3),1)
//  .setCell(Loc(4,1),2)
//  .setCell(Loc(4,2),3)
//  .setCell(Loc(5,1),5)
//  .setCell(Loc(5,2),4)
//  .setCell(Loc(5,3),7)
//
val m1 = Map(
  Loc(1,1) -> Cell(1,2),
  Loc(1,2) -> Cell(2),
  Loc(1,3) -> Cell(3)
)
val m2 = Map(
  Loc(1,1) -> Cell(1,5),
  Loc(1,2) -> Cell(2),
  Loc(1,3) -> Cell(3)
)
def merge( m1: Map[Loc, Cell], m2: Map[Loc, Cell]): Map[Loc, Cell] = {
  m1.foldLeft(m2)(
    (mergedMapping, newMapping) => {
      newMapping match {
        case (loc, cell) =>
          if (mergedMapping.contains(loc)) {
            val currentCellPossibleVals = mergedMapping(loc).possibleVals.toSet
            val newCellPossibleVals = cell.possibleVals.toSet
            val merged = currentCellPossibleVals.intersect(newCellPossibleVals)
            mergedMapping + (loc -> Cell(merged.toList))
          }
          else mergedMapping + (loc -> cell)
      }
    }
  )
}
val t0 = System.nanoTime()
merge(m1, m2)
val elapsed = System.nanoTime()  - t0