package ch.ethz.dal.tinyir.searchEngine

import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.processing.Tokenizer
import scala.collection.mutable
import ch.ethz.dal.tinyir.io.TipsterStream

class LanguageModel(tipster: TipsterStream) extends AbstractModel(tipster) {

  var languageModel: collection.Map[String, Double] = null
		  
  /** Precompute language model: P(w) */
  def computeModel() = {
    var numProcessed = 0
    var cf = mutable.Map[String, Int]().withDefaultValue(0)
    for ( doc <- tipster.stream) {
      cf ++= getCleanTokens(doc.tokens).groupBy(identity).map({ case (term, list) => term -> (list.length + cf.getOrElse(term, 0)) })
      numProcessed += 1
      
      if (numProcessed % 10000 == 0){
        println("Language Model: parsed documents = " + numProcessed)
      }      
    }
    val totalWords = cf.values.sum
    languageModel = cf.mapValues(count => count / totalWords.toDouble)
    println("Language Model: complete")
  }
  
  /** Computes: sum of log P(w|d) = sum of log[ (1-a) * P'(w|d) + a * P(w) ]. */
  protected def computeDocumentScore(query: List[String], tfModel: Map[String, Double]): Double = {
    var score = 0d
    val a = 0.3
    for (queryToken <- query) {
      score += log2(1 + (1 - a) * tfModel.getOrElse(queryToken, 0d) + a * languageModel.getOrElse(queryToken, 0d)) // shift by 1 to get at least 0
    }
    return score
  }
}