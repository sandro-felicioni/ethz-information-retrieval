package ch.ethz.dal.tinyir.searchEngine

import scala.collection.mutable
import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.Tokenizer
import scala.collection.mutable.PriorityQueue

class TermModel(tipster: TipsterStream, numDocumentsToParse: Int = -1) extends AbstractModel(tipster, numDocumentsToParse) {

  var idfModel: collection.Map[String, Double] = null

  /** Precompute IDF Model */
  def computeModel() = {
    var numDocuments = 0
    val idf = mutable.Map[String,Int]().withDefaultValue(0)
    
    for (doc <- getStream()) {
      idf ++= getCleanTokens(doc.tokens).distinct.map(term => term -> (1 + idf.getOrElse(term, 0)))
      numDocuments += 1
      
      if (numDocuments % 10000 == 0){
        println("IDF Model: parsed documents = " + numDocuments)
      }
    }
    
    idfModel = idf.mapValues(count => log2(numDocuments / count.toDouble))
    println("IDF Model: complete")
  }

  protected def computeDocumentScore(query: List[String], tfModel: Map[String, Double], documentTokens: List[String]): Double = {
    // initialize with number of word occurrences in document
    var score : Double = query.map(token => if (tfModel.contains(token)) 1 else 0).sum
    
    for (queryToken <- query) {
      score += logTF(queryToken, tfModel) * idfModel.getOrElse(queryToken, 0d)
    }
    return score
  }
  
  private def logTF(term: String, tfModel: Map[String, Double]) : Double = {
    return log2(1 + tfModel.getOrElse(term, 0d))
  }  
}