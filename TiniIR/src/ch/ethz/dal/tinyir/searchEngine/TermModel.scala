package ch.ethz.dal.tinyir.searchEngine

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.processing.XMLDocument

class TermModel extends AbstractModel{
  var tfModel: Map[String, List[(String, Double)]] = null
  var idfModel: Map[String, Double] = null

  private def computeLogTermFrequency(list: List[(String,Double)]) : List[(String,Double)] = {
    return list.map( { case(docId, tf) => (docId, log2(tf + 1)) }) // we add + 1 so that log is lower bounded by 0
  }

  def computeModel(stream: Stream[XMLDocument]) = {
    tfModel = computeRelativeTermFrequencies(stream).mapValues(list => computeLogTermFrequency(list) )

    val numDocuments = tfModel.values.flatMap(list => list.map(tuple => tuple._1)).toSet.size
    idfModel = tfModel.mapValues(list => log2(numDocuments / list.size.toDouble))
  }

  /**
   * Computes a score for this query and returns the top 100 (at most) relevant documents. Note that just
   * documents are considered, that at least contain one of the words in the query.
   */
  def computeScore(query: String): List[String] = {
    val queryTerms = Tokenizer.toLowerCase(Tokenizer.tokenize(query)).distinct
    val length = queryTerms.length

    // compute scores for each document which contains at least one word in query
    var scores = new mutable.HashMap[String, Double]()
    queryTerms.foreach(term => tfModel.getOrElse(term, List()).foreach { case (docId, tf) => scores.put(docId, tf * idfModel.get(term).get + scores.getOrElse(docId, 0d)) })

    // sort after highest score and return at most the top 100 relevant documents 
    val topScores = scores.toList.sortBy({ case (docId, score) => -score }).take(100)
    //topScores.foreach(pair => println("document id = " + pair._1 + "\tscore = " + pair._2))

    return topScores.map({ case (docId, score) => docId })
  }
}