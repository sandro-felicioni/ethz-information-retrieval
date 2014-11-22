package ch.ethz.dal.classifier

import ch.ethz.dal.processing.ReutersCorpusIterator
import scala.collection

class NaiveBayesClassifier(datasetPath: String) {

  /* class probabilities P(c) */
  var classProbabilities = collection.mutable.Map[String, Double]()

  /* unnormalized conditional probabilities P(w | c) */
  var conditionalProbabilities = collection.mutable.Map[String, collection.mutable.Map[String, Int]]()
  
  /* normalization constant per class (stored separately due to smoothing)*/
  var numWordsPerClass = Map[String, Int]()
  
  private def computeClassProbabilities() = {
    val topicCounts = collection.mutable.Map[String, Int]()
    var numDocuments = 0
    val documentIterator = new ReutersCorpusIterator(datasetPath)
    while (documentIterator.hasNext) {
      val doc = documentIterator.next
      topicCounts ++= doc.topics.map(c => (c -> (1 + topicCounts.getOrElse(c, 0))))
      numDocuments = numDocuments + 1
    }
    topicCounts.keys.foreach(topic => classProbabilities.put(topic, topicCounts.getOrElse(topic, 0) / numDocuments.toDouble))

    for ((topic, probability) <- classProbabilities) {
      // println(topic + ": " + probability)
    }
    println("Number of documents: " + numDocuments)
    println("Number of topics: " + classProbabilities.keys.size)
    println("Computed class probabilities P(c)")
  }

  private def computeConditionalProbabilities() = {
    // initialize all conditional probabilities
    classProbabilities.keys.foreach(topic => conditionalProbabilities.put(topic, collection.mutable.Map[String, Int]()))
    
    val documentIterator = new ReutersCorpusIterator(datasetPath)
    while (documentIterator.hasNext) {
      val doc = documentIterator.next
      val docLength = doc.tokens.size
      val termFrequency = doc.tokens.groupBy(identity).mapValues(valueList => valueList.length)

      for (topic <- doc.topics) {
        var conditionalProbability = conditionalProbabilities.get(topic).get
        conditionalProbability ++= termFrequency.map { case (term, frequency) => term -> (frequency + conditionalProbability.getOrElse(term, 0)) }
        numWordsPerClass += topic -> (docLength + numWordsPerClass.getOrElse(topic, 0))
      }
    }
  }
  
  def training() = {
    computeClassProbabilities();
    computeConditionalProbabilities();
    
    // TODO: stop words removal
    // TODO: stemming 
    // TODO: smoothing
  }
  
  def predict(testset: ReutersCorpusIterator) = {
    
  } 
}