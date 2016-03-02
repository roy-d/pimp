package lab

case class Column(rows: List[Int]){
  def get(colIndex: Int): Int = rows(colIndex)

  val dimension: Int = rows.size
}

case class Row(cols: List[Int]) {
  def get(colIndex: Int): Int = cols(colIndex)

  val dimension: Int = cols.size
}

object Row {
  def fromString(rawRow: String) = apply(rawRow.split(",").toList.map(_.toInt))
}

case class Matrix(rows: List[Row]) {
  def get(rowIndex: Int): Row = rows(rowIndex)

  def get(rowIndex: Int, colIndex: Int): Int = get(rowIndex).get(colIndex)

  val dimension: (Int, Int) = (rows.size, rows.head.dimension)

  val columns: List[Column] = {
    val (_, colSize) = dimension

    (0 until colSize).map {
      colIndex => Column(rows.map(row => row.get(colIndex)))
    }.toList
  }

}

object Matrix {
  def fromString(rawMatrix: String) = apply(rawMatrix.split(";").toList.map(Row.fromString))
}

package object pimp {

  implicit class MatrixOps(matrix: Matrix) {
    def add(aMatrix: Matrix): Matrix = {
      if (matrix.dimension == aMatrix.dimension) {
        Matrix(
          matrix.rows.zip(aMatrix.rows).map { case (row1, row2) =>
            Row(
              row1.cols.zip(row2.cols).map { case (c1, c2) => c1 + c2 }
            )
          }
        )
      } else {
        sys.error("dimensions don't match")
      }
    }

    def transpose(): Matrix = Matrix(matrix.columns.map(column=>Row(column.rows)))

  }
}