package ch.ethz.dal.tinyir.searchEngine

import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.processing.XMLDocument
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TermModel {
  var tfModel: Map[String, List[(String, Double)]] = null
  var idfModel: Map[String, Double] = null

  /**
   * Compute the log-tf score of a particular word in a particular document.
   *
   * @param frequency is the absolute frequency of a particular word within a document
   * @param totalWords is the total number of words within a document
   * @return the log tf score
   */
  private def logTermFrequency(termFrequency: Int, totalWords: Double): Double = {
    return math.log((termFrequency / totalWords) + 1) // we add + 1 so that log is lower bounded by 0 
  }

  /** Create a list of triples (token, doc-id, relative-frequency) for term frequency within a document */
  private def createFrequencyTriples(stream: Stream[XMLDocument]): List[(String, String, Double)] = {
    return stream.flatMap(doc => doc.tokens.groupBy(identity)
      .map { case (token, list) => (token, doc.name, logTermFrequency(list.length, doc.tokens.size)) })
      .toList
  }

  def computeTFIDFModel(stream: Stream[XMLDocument]) = {

    // FIXME total number of documents (note, if you call size then it first processes all documents..)
    val numDocuments = 500 //tipster.stream.size

    // create a map[term, List[(doc-id, frequency)]] (an entry is only created if a document has at least once a particular term)
    tfModel = createFrequencyTriples(stream.take(numDocuments))
      .groupBy(triple => triple._1)
      .mapValues(list => list.map(triple => (triple._2, triple._3)));

    idfModel = tfModel.mapValues(list => math.log(numDocuments / list.size.toDouble))
    println("number of terms = " + tfModel.keys.size)
  }

  /**
   * Computes a score for this query and returns the top 100 (at most) relevant documents. Note that just
   * documents are considered, that at least contain one of the words in the query.
   */
  def scoreTFIDF(query: String): List[String] = {
    val queryTerms = Tokenizer.tokenize(query).distinct
    val length = queryTerms.length

    // compute scores for each document which contains at least one word in query
    var scores = new mutable.HashMap[String, Double]()
    queryTerms.foreach(term => tfModel.getOrElse(term, List()).foreach { case (docId, tf) => scores.put(docId, tf * idfModel.get(term).get + scores.getOrElse(docId, 0d)) })

    // sort after highest score and return at most the top 100 relevant documents 
    val topScores = scores.toList.sortBy({ case (docId, score) => -score }).take(100)
    topScores.foreach(pair => println("document id = " + pair._1 + "\tscore = " + pair._2))

    return topScores.map({ case (docId, score) => docId })
  }

  // Legacy - Loop based TF
  def processSlow(stream: Stream[XMLDocument]) = {
    var tf: scala.collection.mutable.Map[String, ListBuffer[(String, Double)]] = scala.collection.mutable.Map()
    for (doc <- stream.take(1000)) {
      for ((term, frequency) <- doc.tokens.groupBy(identity).mapValues(list => list.length / doc.tokens.size.toDouble)) {

        if (!tf.contains(term)) {
          tf += (term -> new ListBuffer[(String, Double)]())
        }

        tf(term) :+= (doc.name, frequency)
      }
    }
    println("number of terms = " + tf.keys.size)
  }
}