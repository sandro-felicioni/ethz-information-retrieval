package ch.ethz.dal.classifier

import ch.ethz.dal.processing.ReutersCorpusIterator
import ch.ethz.dal.util.StopWatch
import ch.ethz.dal.util.Scores
import java.io.FileOutputStream
import java.io.PrintWriter
import java.util.Date

object Main {

  def main(args: Array[String]) {
    val rootPath = "./reuters-dataset"
    var trainingSet = rootPath + "/train"
    var validationSet = rootPath + "/test-with-labels"
    var testSet = rootPath + "/test-without-labels"
    var classifierToUse = "svm"

    if (args.length == 4) {
      trainingSet = args(0)
      validationSet = args(1)
      testSet = args(2)
      if (args(3) == "nb" || args(3) == "lr" || args(3) == "svm") {
        classifierToUse = args(3)
      }
    } else {
      println("Not enough arguments (default values are now used), provide exactly 4 arguments:")
      println("\tpath to training set: Default value = " + trainingSet)
      println("\tpath to validation set (= test set with labeled data): Default value = " + validationSet)
      println("\tpath to test set(= test set without labeled data): Default value = " + testSet)
      println("\tThe model you want to use (either nb, lr or svm): Default value = " + classifierToUse)
    }

    // for test purposes we use a restricted set of topics to classify
    var validationTopics = List[String]("MCAT", "E121", "M141", "GFAS", "M12", "C174", "C24", "GCRIM", "GVIO", "GDIP", "E14", "C13", "E211", "GSPO", "E142", "GENT", "GCAT", "E311", "G156", "C31", "E131", "E511", "E512", "GWELF", "GPRO", "E313", "C42", "C1511", "G154", "C17")

    var classifier: AbstractClassifier = null
    if (classifierToUse == "nb") {
      classifier = new NaiveBayesClassifier(trainingSet, Set[String](), 3, 1, true, false)
    } else if (classifierToUse == "lr") {
      classifier = new LogisticRegressionClassifier(trainingSet, validationTopics.toSet, threshold = 0.75, true, true)
    } else if (classifierToUse == "svm") {
      classifier = new SVMClassifier(trainingSet, validationTopics.toSet, lambda = 0.005, true, true)
    }

    // training
    var watch = new StopWatch()
    watch.start
    classifier.training()
    watch.stop
    println("training time = " + watch.stopped)

    // predict validation set
    predictValidationSet(classifier, classifierToUse, validationSet)

    // predict test set
    predictTestSet(classifier, classifierToUse, testSet)
  }

  def predictValidationSet(classifier: AbstractClassifier, classifierToUse: String, validationSet: String) = {
    var watch = new StopWatch()
    watch.start

    var documentClassifications = classifier.predict(validationSet)
    val scores = new Scores()
    var validationIterator = new ReutersCorpusIterator(validationSet)
    for (document <- validationIterator) {
      val predictedTopics = documentClassifications.get(document.ID).get
      val trueTopics = classifier.filterTopics(document.topics)
      scores.addScores(predictedTopics, trueTopics)
    }
    println(scores.getAveragePrecision + " " + scores.getAverageRecall + " " + scores.getAverageF1)
    writeToFile(classifierToUse, "l", documentClassifications, scores)

    watch.stop
    println("predicting validation-set time = " + watch.stopped)
  }

  def predictTestSet(classifier: AbstractClassifier, classifierToUse: String, testSet: String) = {
    var watch = new StopWatch()
    watch.start

    var documentClassifications = classifier.predict(testSet)
    writeToFile(classifierToUse, "u", documentClassifications, null)

    watch.stop
    println("predicting test-set time = " + watch.stopped)
  }

  def writeToFile(classifierToUse: String, dataset: String, allPredictedTopics: Map[Int, Set[String]], scores: Scores): Unit = {
    val writer = new PrintWriter(new FileOutputStream("classify-" + "sandro-felicioni-" + dataset + "-" + classifierToUse + ".run"))

    // if it is a labeled dataset print also the scores
    if (dataset == "l") {
      writer.println(scores.getAveragePrecision + " " + scores.getAverageRecall + " " + scores.getAverageF1)
    }

    for ((docId, topics) <- allPredictedTopics) {
      writer.println(docId + " " + topics.mkString(" "))
    }
    writer.flush()
    writer.close()
  }

}