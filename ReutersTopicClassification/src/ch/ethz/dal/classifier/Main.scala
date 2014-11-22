package ch.ethz.dal.classifier

import ch.ethz.dal.processing.ReutersCorpusIterator
import ch.ethz.dal.util.StopWatch
import ch.ethz.dal.util.Scores

object Main {

  def main(args: Array[String]) {
    val rootPath = "./reuters-dataset"
    val training_set = rootPath + "/train"
    val validation_set = rootPath + "/test-with-labels"
    
    val watch = new StopWatch()
    watch.start
    var bayesClassifier = new NaiveBayesClassifier(training_set, 5)
    bayesClassifier.training
    watch.stop
    println("training time = " + watch.stopped)
    
    var documentClassifications = bayesClassifier.predict(validation_set)
    val validationIterator = new ReutersCorpusIterator(validation_set)
    val scores = new Scores()
    for(document <- validationIterator){
      val predictedTopics = documentClassifications.get(document.ID).get
      val trueTopics = document.topics
      scores.addScores(predictedTopics, trueTopics)
    }
    println(scores.getAveragePrecision + " " + scores.getAverageRecall + " " + scores.getAverageF1)
    
    watch.stop
    println("predicting validation-set time = " + watch.stopped)
    
    
  }
}