package ch.ethz.dal.classifier

import ch.ethz.dal.processing.ReutersCorpusIterator
import ch.ethz.dal.util.StopWatch
import ch.ethz.dal.util.Scores

object Main {

  def main(args: Array[String]) {
    val rootPath = "./reuters-dataset"
    val training_set = rootPath + "/train"
    val validation_set = rootPath + "/test-with-labels"
    val classifierToUse = "svm"
    
    // for test purposes we use a restricted set of topics to classify
    var restrictedTopics = List[String]("MCAT", "E21", "GPOL", "GPRO", "GSCI", "GJOB", "E141", "E312", "C42", "M142")
    var validationTopics = List[String]("MCAT", "E121", "M141", "GFAS", "M12", "C174", "C24", "GCRIM", "GVIO", "GDIP", "E14", "C13", "E211", "GSPO", "E142", "GENT", "GCAT", "E311", "G156", "C31", "E131", "E511", "E512", "GWELF", "GPRO", "E313", "C42", "C1511", "G154", "C17")
    
    var classifier : AbstractClassifier = null
    if (classifierToUse == "nb"){
      classifier = new NaiveBayesClassifier(training_set, Set[String](), 3, 1, true, false)
    }else if( classifierToUse == "lg"){
      classifier = new LogisticRegressionClassifier(training_set, validationTopics.toSet, threshold=0.75, true, true)
    }else if( classifierToUse == "svm"){
      classifier = new SVMClassifier(training_set, validationTopics.toSet, lambda=0.005, true, true)
    }
    
    val watch = new StopWatch()
    watch.start
    classifier.training
    watch.stop
    println("training time = " + watch.stopped)
    
    var documentClassifications = classifier.predict(validation_set)
    val validationIterator = new ReutersCorpusIterator(validation_set)
    val scores = new Scores()
    for(document <- validationIterator){
      val predictedTopics = documentClassifications.get(document.ID).get
      val trueTopics = classifier.filterTopics(document.topics)
      scores.addScores(predictedTopics, trueTopics)
      
//      if (math.random < 0.001){
//        println("num predicted topics = " + predictedTopics + " num true topics = " + trueTopics)
//      }
    }
    println(scores.getAveragePrecision + " " + scores.getAverageRecall + " " + scores.getAverageF1)
    
    watch.stop
    println("predicting validation-set time = " + watch.stopped)
    
    
  }
}