package ch.ethz.dal.tinyir.searchEngine

import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.processing.Tokenizer
import scala.collection.mutable

class LanguageModel extends AbstractModel {

  var tfModel: Map[String, List[(String, Double)]] = null
  var languageModel : Map[String, Double] = null
  
  private def convertAbsoluteToRelativeFrequencies(list: List[(String, Int, Int)]) : List[(String, Double)] = {
    return list.map(triple => (triple._1, triple._2 / triple._3.toDouble ))
  }
  
  /** Get a list of (docId, absolute-frequency, document-size) for a particular term */
  private def countWordsInCollection(list: List[(String, Int, Int)]) : Int = {
    return list.map(triple => triple._2).sum
  }
  
  def computeModel(stream: Stream[XMLDocument]) = {
    val absoluteTfModel = computeAbsoluteTermFrequencies(stream)
    val totalWords = absoluteTfModel.flatMap(tfEntry => tfEntry._2.map(triple => triple._2)).sum
    
    languageModel = absoluteTfModel.mapValues(list => countWordsInCollection(list)/totalWords.toDouble)
    tfModel = absoluteTfModel.mapValues(list => convertAbsoluteToRelativeFrequencies(list))
  }
  
  /** Computes log P(w|d) = log[ (1-a) * P'(w|d) + a * P(w) ]. */
  private def termScorePerDocument(termFrequency: Double, term: String) : Double = {
    val a = 0.7
    return log2(1 + (1-a) * termFrequency + a * languageModel.get(term).get) // shift by 1 to get at least 0 
  }
  
  def computeScore(query: String): List[String] = {
    val queryTerms = Tokenizer.toLowerCase(Tokenizer.tokenize(query)).distinct
    val length = queryTerms.length

    // compute scores for each document which contains at least one word in query
    var scores = new mutable.HashMap[String, Double]()
    queryTerms.foreach(term => tfModel.getOrElse(term, List()).foreach { case (docId, tf) => scores.put(docId, termScorePerDocument(tf, term) + scores.getOrElse(docId, 0d)) })

    // sort after highest score and return at most the top 100 relevant documents 
    val topScores = scores.toList.sortBy({ case (docId, score) => -score }).take(100)
    //topScores.foreach(pair => println("document id = " + pair._1 + "\tscore = " + pair._2))

    return topScores.map({ case (docId, score) => docId })
  }
}