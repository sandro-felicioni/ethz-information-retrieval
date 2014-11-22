package ch.ethz.dal.classifier

import ch.ethz.dal.processing.ReutersCorpusIterator
import ch.ethz.dal.util.StopWatch

object Main {

  def main(args: Array[String]) {
    val rootPath = "./reuters-dataset"
    val training_set = rootPath + "/train"
    
    val watch = new StopWatch()
    watch.start
    var bayesClassifier = new NaiveBayesClassifier(training_set)
    bayesClassifier.training
    watch.stop
    println("time = " + watch.stopped)
  }
}