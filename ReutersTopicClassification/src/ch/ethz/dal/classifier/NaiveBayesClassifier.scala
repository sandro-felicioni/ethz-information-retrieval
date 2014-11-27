package ch.ethz.dal.classifier

import ch.ethz.dal.processing.ReutersCorpusIterator
import scala.collection
import ch.ethz.dal.processing.ReutersRCVParse
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ListBuffer
import com.github.aztek.porterstemmer.PorterStemmer

class NaiveBayesClassifier(datasetPath: String, restrictedTopics: Set[String], maxDocuments: Int, alpha: Int, removeStopwords: Boolean, useStemming: Boolean) extends AbstractClassifier(restrictedTopics, removeStopwords, useStemming){

  /* All topics that are available */
  var topics = Iterable[String]()

  /* class probabilities P(c) */
  var classProbabilities = collection.mutable.Map[String, Double]()

  /* unnormalized conditional probabilities P(w | c) */
  var conditionalProbabilities = collection.mutable.Map[String, collection.mutable.Map[String, Int]]()

  /* normalization constant per class (stored separately due to smoothing)*/
  var numWordsPerClass = Map[String, Int]()
  
  /* the vocabulary that is used */
  var vocabulary = Set[String]()

  private def computeClassProbabilities() = {
    val topicCounts = collection.mutable.Map[String, Int]()
    var numDocuments = 0
    val documentIterator = new ReutersCorpusIterator(datasetPath)
    for (doc <- documentIterator) {
      topicCounts ++= doc.topics.map(c => (c -> (1 + topicCounts.getOrElse(c, 0))))
      numDocuments = numDocuments + 1
    }
    topicCounts.keys.foreach(topic => classProbabilities.put(topic, topicCounts.getOrElse(topic, 0) / numDocuments.toDouble))

    topics = classProbabilities.keys

    println("Number of documents: " + numDocuments)
    println("Number of topics: " + classProbabilities.keys.size)
    println("Computed class probabilities P(c)")
  }

  private def computeConditionalProbabilities() = {
    // initialize all conditional probabilities
    classProbabilities.keys.foreach(topic => conditionalProbabilities.put(topic, collection.mutable.Map[String, Int]()))

    val documentIterator = new ReutersCorpusIterator(datasetPath)
    for (doc <- documentIterator) {
      val cleanTokens = getCleanTokens(doc.tokens) 
      val docLength = cleanTokens.size
      val termFrequency = cleanTokens.groupBy(identity).mapValues(valueList => valueList.length)
      vocabulary  ++= termFrequency.keySet

      for (topic <- doc.topics) {
        var conditionalProbability = conditionalProbabilities.get(topic).get
        conditionalProbability ++= termFrequency.map { case (term, frequency) => term -> (frequency + conditionalProbability.getOrElse(term, 0)) }
        numWordsPerClass += topic -> (docLength + numWordsPerClass.getOrElse(topic, 0))
      }
    }
     println("Computed conditional probabilities P(w|c)")
  }

  private def predictTopicsForDocument(doc: ReutersRCVParse): Set[String] = {
    val termFrequency = getCleanTokens(doc.tokens).groupBy(identity).mapValues(valueList => valueList.length)
    
    var priorityQueue = new collection.mutable.PriorityQueue[(String, Double)]()(Ordering.by(score => -score._2))
    for (topic <- topics) {
    	var score = log2(classProbabilities.get(topic).get)
    	val conditionalProbability = conditionalProbabilities.get(topic).get
    	val numWords = numWordsPerClass.get(topic).get.toDouble
    	
    	for (term <- termFrequency.keys){
    	  score += termFrequency.get(term).get * log2(( (alpha + conditionalProbability.getOrElse(term, 0)) / (numWords + alpha*vocabulary.size) ))
    	}
    	
    	if(priorityQueue.size < maxDocuments || score > priorityQueue.head._2){
    	  priorityQueue.enqueue((topic, score))
    	}
    	
    	if(priorityQueue.size > maxDocuments){
    	  priorityQueue.dequeue
    	}
    }
    
    return priorityQueue.map{case (topic, score) => topic}.toSet
  }
  
  def training() = {
    computeClassProbabilities();
    computeConditionalProbabilities();
  }

  def predict(testsetPath: String) : Map[Int, Set[String]] = {
    var documentClassifications = Map[Int, Set[String]]()
    val documentIterator = new ReutersCorpusIterator(testsetPath)
    for (doc <- documentIterator) {
      val documentClassification = predictTopicsForDocument(doc)
      documentClassifications += ((doc.ID, documentClassification))
    }
    
    return documentClassifications
  }
}