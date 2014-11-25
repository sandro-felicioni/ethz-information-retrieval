package ch.ethz.dal.classifier

import breeze.linalg.DenseVector
import breeze.linalg.CSCMatrix
import breeze.linalg.SparseVector

abstract class AbstractClassifier {

  /** Compute log_2 */
  protected def log2(x: Double): Double = {
    return math.log(x) / math.log(2)
  }

  /** Since breeze's CSC doesn't provide a method to extract/slice one single column I created an own method */
  def extractColumn(sparseMatrix: CSCMatrix[Double], columnIndex: Int): SparseVector[Double] = {
	val column = SparseVector.zeros[Double](sparseMatrix.rows)

    val startIndex = sparseMatrix.colPtrs(columnIndex)
    val endIndex = sparseMatrix.colPtrs(columnIndex + 1)
    for (idx <- Range(startIndex, endIndex)) {
      val row_idx = sparseMatrix.rowIndices(idx);
      val value = sparseMatrix.data(idx)
      column(row_idx) = value
    }
    return column
  }

  def training()

  def predict(testsetPath: String): Map[Int, Set[String]]
}