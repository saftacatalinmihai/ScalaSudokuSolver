val p = Puzzle()
p.cells
p.setCell(Loc(1,1),1)
  .setCell(Loc(1,2),2)
  .setCell(Loc(1,3),3)
  .setCell(Loc(2,1),4)
  .setCell(Loc(2,2),5)
  .setCell(Loc(2,3),6)
  .setCell(Loc(3,1),7)
  .setCell(Loc(3,2),8)
  .setCell(Loc(4,3),1)
  .setCell(Loc(4,1),2)
  .setCell(Loc(4,2),3)
  .setCell(Loc(5,1),5)
  .setCell(Loc(5,2),4)
  .setCell(Loc(5,3),7)

def zero_? (i: Int) : Boolean = i == 0

zero_?(1)
zero_?(0)