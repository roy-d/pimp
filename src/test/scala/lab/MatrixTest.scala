package lab

import org.scalatest.{FlatSpec, MustMatchers}
import pimp.MatrixOps

class MatrixTest extends FlatSpec with MustMatchers {

  "Matrix" should "return dimensions" in {
    val matrix = Matrix.fromString("1,2,3;4,5,6;")
    val (row,col) = matrix.dimension
    row must === (2)
    col must === (3)
  }

  "Matrix" should "return sum with another Matrix" in {
    val matrix = Matrix.fromString("1,1,1;1,1,1;")
    val sum = matrix.add(Matrix.fromString("1,1,1;1,1,1;"))
    sum.get(0,0) must === (2)
  }

  "Matrix" should "throw error if sum doesn't match dimension" in {
    val matrix = Matrix.fromString("1,1,1;1,1,1;1,1,1;")
    intercept[RuntimeException]{matrix.add(Matrix.fromString("1,1,1;1,1,1;"))}
  }

  "Matrix" should "return transpose" in {
    val matrix = Matrix.fromString("1,2,3;4,5,6;")
    val trans = matrix.transpose()
    val (row,col) = trans.dimension
    row must === (3)
    col must === (2)
    trans.get(0,0) must === (1)
    trans.get(0,1) must === (4)
    trans.get(1,0) must === (2)
  }

}
