package ch.ethz.dal.util

import scala.collection.mutable.ListBuffer

class Scores {

  var precisions = ListBuffer[Double]()
  var recalls = ListBuffer[Double]()
  var f1s = ListBuffer[Double]()

  private def getPrecision(predictedTopics: Set[String], trueTopics: Set[String]): Double = {
    val numRetrieved = predictedTopics.size
    if (numRetrieved == 0) {
      return 0
    }
    return trueTopics.intersect(predictedTopics).size / numRetrieved.toDouble
  }

  private def getRecall(predictedTopics: Set[String], trueTopics: Set[String]): Double = {
    val numRelevant = trueTopics.size
    if (numRelevant == 0) {
      return 0
    }
    return trueTopics.intersect(predictedTopics).size / numRelevant.toDouble
  }

  private def getF1(precision: Double, recall: Double): Double = {
    if (precision + recall == 0){
      return 0
    }
    return 2 * (precision * recall) / (precision + recall)
  }

  def addScores(predictedTopics: Set[String], trueTopics: Set[String]) = {
    val precision = getPrecision(predictedTopics, trueTopics)
    val recall = getRecall(predictedTopics, trueTopics)
    val f1 = getF1(precision, recall)
    
    // ignore documents without any true topics assigned to them, since it falsifies the result with 0-entries and makes it less interpretable
    if (! trueTopics.isEmpty){
	    precisions += precision
	    recalls += recall
	    f1s += f1
    }
  }

  def getAveragePrecision(): Double = {
    return precisions.sum / precisions.size.toDouble
  }

  def getAverageRecall(): Double = {
    return recalls.sum / recalls.size.toDouble
  }

  def getAverageF1(): Double = {
    return f1s.sum / f1s.size.toDouble
  }
}