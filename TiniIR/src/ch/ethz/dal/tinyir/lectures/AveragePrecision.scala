package ch.ethz.dal.tinyir.lectures
import scala.collection.Seq

class AveragePrecision[A](rankedDocuments: Seq[A], relevantDocuments: Set[A], k: Int) {

  // take at most k ranked documents
  private val ranked: Seq[A] = if (rankedDocuments.length > k) rankedDocuments.take(k) else rankedDocuments

  // compute average precision
  def avgPrecision(): Double = {
    var numHit = 0
    val precisions = (ranked zip Stream.from(1)).map {
				        case (prediction, index) =>
				          if (relevantDocuments.contains(prediction)) {
				            numHit += 1
				            numHit / index.toDouble  // computes precision numRelevance/numRetrieved so far
				          } else {
				            0
				          }
      				}

    val ap = if (relevantDocuments.size == 0) 0d else (precisions.sum / math.min(relevantDocuments.size, k).toDouble)
    return ap
  }
}

object AveragePrecision {
  def main(args: Array[String]) {

    val ranked1: Seq[Int] = Seq(1, 2)
    val relevance1: Set[Int] = Set(1, 2, 3)
    val k1 = 3;

    println(new AveragePrecision(ranked1, relevance1, 3).avgPrecision)
    assert(new AveragePrecision(ranked1, relevance1, 3).avgPrecision == 0.6666666666666666d)

    val ranked2: Seq[Int] = Seq(1, 2, 3)
    val relevance2: Set[Int] = Set(1, 3, 6)
    val k2 = 3;

    println(new AveragePrecision(ranked2, relevance2, k2).avgPrecision)
    assert(new AveragePrecision(ranked2, relevance2, k2).avgPrecision == 0.5555555555555555d)

    val ranked3: Seq[Int] = Seq(1, 2, 3)
    val relevance3: Set[Int] = Set(1, 3, 6)
    val k3 = 2;

    println(new AveragePrecision(ranked3, relevance3, k3).avgPrecision)
    assert(new AveragePrecision(ranked3, relevance3, k3).avgPrecision == 0.5d)

    val ranked4: Seq[Int] = Seq(1, 3, 2)
    val relevance4: Set[Int] = Set(1, 3, 6)
    val k4 = 2;

    println(new AveragePrecision(ranked4, relevance4, k4).avgPrecision)
    assert(new AveragePrecision(ranked4, relevance4, k4).avgPrecision == 1)
  }
} 