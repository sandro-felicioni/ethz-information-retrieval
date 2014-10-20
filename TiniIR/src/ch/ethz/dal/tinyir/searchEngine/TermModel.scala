package ch.ethz.dal.tinyir.searchEngine

import scala.collection.mutable
import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.Tokenizer
import scala.collection.mutable.PriorityQueue

class TermModel(tipster: TipsterStream) extends AbstractModel(tipster) {

  var idfModel: collection.Map[String, Double] = null

  /** Precompute IDF Model */
  def computeModel() = {
    var numDocuments = 0
    val idf = mutable.Map[String,Int]().withDefaultValue(0)
    
    for (doc <- tipster.stream.take(30000)) {
      idf ++= getCleanTokens(doc.tokens).distinct.map(term => term -> (1 + idf.getOrElse(term, 0)))
      numDocuments += 1
      
      if (numDocuments % 10000 == 0){
        println("IDF Model: parsed documents = " + numDocuments)
      }
    }
    
    idfModel = idf.mapValues(count => log2(numDocuments / count.toDouble))
    println("IDF Model: complete")
  }

  protected def computeDocumentScore(query: String, tfModel: Map[String, Double]): Double = {
    var score = 0d
    for (queryToken <- getCleanTokens(Tokenizer.tokenize(query)).distinct) {
      score += logTF(queryToken, tfModel) * idfModel.getOrElse(queryToken, 0d)
    }
    return score
  }
  
  private def logTF(term: String, tfModel: Map[String, Double]) : Double = {
    return log2(1 + tfModel.getOrElse(term, 0d))
  }  
}