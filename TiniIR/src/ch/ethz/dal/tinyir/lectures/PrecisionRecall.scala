
package ch.ethz.dal.tinyir.lectures

import collection.Seq
import util.Random
import math.{ min, max }
import javax.sound.midi.Sequence

class PrecisionRecall[A](ranked: Seq[A], relev: Set[A]) {

  // total number of relevant items  
  val totalRelevantDocs = relev.size

  // the indices in the ranked sequence at which relevant items occur
  val relevIdx = ranked.zipWithIndex.filter { case (r, _) => relev(r) }.map(_._2).toArray

  // the precision values at index positions relevIdx
  val precs = relevIdx.zipWithIndex.map { case (rank, rel) => (rel + 1) / (rank + 1).toDouble }

  // interpolation of precision to all recall levels 
  val iprecs = precs.scanRight(0.0)((a, b) => Math.max(a, b)).dropRight(1)

  // number of results to reach recall level 
  private def recall2num(recall: Double) = {
    assert(recall >= 0.0 && recall <= 1.0)
    min((recall * totalRelevantDocs).ceil.toInt, totalRelevantDocs)
  }

  // precision at recall level 
  def precAt(recall: Double, interpolated: Boolean = false) = {
    assert(recall >= 0.0 && recall <= 1.0)
    val n = max(1, recall2num(recall))
    if (interpolated) iprecs(n - 1)
    else precs(n - 1)
  }

  // Equally spaced recall levels between 0 and 1
  private def recallLevels(nLevels: Int) = {
    (0 to nLevels - 1).map(i => i / (nLevels - 1).toDouble)
  }

  // Precision at recall levels
  def getInterpolatedPrecisionAt(nRecallLevels: Int) = {
    recallLevels(nRecallLevels).map(precAt(_, true))
  }

  // Averaged precision
  def getAveragePrecision(nRecallLevels: Int) : Double = {
    return getInterpolatedPrecisionAt(nRecallLevels).sum / nRecallLevels
  }
}

object PrecisionRecall {

  case class PrecRec(precision: Double, recall: Double) {
    def mkstr: String = "P = " + precision + ", R = " + recall
  }

  def evaluate[A](retriev: Set[A], relev: Set[A]) = {
    val truePos = (retriev & relev).size
    PrecRec(
      precision = truePos.toDouble / retriev.size.toDouble,
      recall = truePos.toDouble / relev.size.toDouble)
  }

  def main(args: Array[String]) = {
    //    {
    //      val relevant   = Set(3,6,7,8,9)
    //      val retrieved  = Set(1,2,3,6) 
    //	  println(PrecisionRecall.evaluate(retrieved, relevant).mkstr)
    //    }

    //    {
    //      val relevant = Set(3,7,9,15,19)
    //      val ranked = Random.shuffle((0 to 19).toList)
    //
    //      val pr = new PrecisionRecall(ranked,relevant)
    //      println(pr.relevIdx.mkString(" "))
    //      println(pr.precs.mkString(" "))
    //      println(pr.iprecs.mkString(" "))
    //      
    //      val recall = 0.65
    //      println("Precision (non interp. ) at " + recall +" = " + pr.precAt(recall,false))
    //      println("Precision (interpolated) at " + recall +" = " + pr.precAt(recall,true))
    //    }

    val relevant: Set[Int] = Set(2, 4, 6)
    val ranked: List[Int] = List(2, 1, 3, 4, 5, 6)
    val precisionRecall = new PrecisionRecall(ranked, relevant)

    println("Precision at relevant indices : " + precisionRecall.precs.mkString(" "))
    println("Precision at 11 recall levels : " + precisionRecall.getInterpolatedPrecisionAt(11))
    println("Average Precision : " + precisionRecall.getAveragePrecision(11))
    
  }
}

/** Experimental - untested */
class PrecisionRecallCurve(num: Int) {

  def evaluate[A](ranked: Iterable[A], relev: Set[A]): Iterable[Double] = {
    val nrelev = relev.size.toDouble
    val binary = ranked.map(doc => if (relev(doc)) 1 else 0)
    val cummul = binary.scanLeft(0)(_ + _).drop(1).map(_ / nrelev)

    return cummul
  }
}
