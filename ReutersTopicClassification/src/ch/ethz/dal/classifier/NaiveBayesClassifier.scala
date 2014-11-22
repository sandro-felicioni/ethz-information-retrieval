package ch.ethz.dal.classifier

import ch.ethz.dal.processing.ReutersCorpusIterator
import scala.collection
import ch.ethz.dal.processing.ReutersRCVParse
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ListBuffer

class NaiveBayesClassifier(datasetPath: String, maxDocuments: Int) {

  var topics = Iterable[String]()

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
    for (doc <- documentIterator) {
      topicCounts ++= doc.topics.map(c => (c -> (1 + topicCounts.getOrElse(c, 0))))
      numDocuments = numDocuments + 1
    }
    topicCounts.keys.foreach(topic => classProbabilities.put(topic, topicCounts.getOrElse(topic, 0) / numDocuments.toDouble))

    topics = classProbabilities.keys

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
    for (doc <- documentIterator) {
      val docLength = doc.tokens.size
      val termFrequency = doc.tokens.groupBy(identity).mapValues(valueList => valueList.length)

      for (topic <- doc.topics) {
        var conditionalProbability = conditionalProbabilities.get(topic).get
        conditionalProbability ++= termFrequency.map { case (term, frequency) => term -> (frequency + conditionalProbability.getOrElse(term, 0)) }
        numWordsPerClass += topic -> (docLength + numWordsPerClass.getOrElse(topic, 0))
      }
    }
     println("Computed conditional probabilities P(w|c)")
  }

  private def predictTopicsForDocument(doc: ReutersRCVParse, threshold: Double): Set[String] = {
    val termFrequency = doc.tokens.groupBy(identity).mapValues(valueList => valueList.length)
    
    var priorityQueue = new collection.mutable.PriorityQueue[(String, Double)]()(Ordering.by(score => -score._2))
    for (topic <- topics) {
    	var score = classProbabilities.get(topic).get
    	val conditionalProbability = conditionalProbabilities.get(topic).get
    	val numWords = numWordsPerClass.get(topic).get
    	
    	for (term <- termFrequency.keys){
    	  score += conditionalProbability.getOrElse(term, 0) * log2((conditionalProbability.getOrElse(term, 0) / numWords.toDouble))
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
  
  /** Compute log_2 */
  protected def log2(x: Double): Double = {
    return math.log(x) / math.log(2)
  }

  def training() = {
    computeClassProbabilities();
    computeConditionalProbabilities();

    // TODO: stop words removal
    // TODO: stemming 
    // TODO: smoothing
  }

  def predict(testsetPath: String) : Map[Int, Set[String]] = {
    val threshold = 0.2
    var documentClassifications = Map[Int, Set[String]]()
    val documentIterator = new ReutersCorpusIterator(testsetPath)
    for (doc <- documentIterator) {
      val documentClassification = predictTopicsForDocument(doc, threshold)
      documentClassifications += ((doc.ID, documentClassification))
    }
    
    return documentClassifications
  }
}