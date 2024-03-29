package ch.ethz.dal.tinyir.alerts

import ch.ethz.dal.tinyir.processing.Tokenizer

/**
 * Representation of a query. 
 */
class Query (query: String) {  
  val qterms = Tokenizer.tokenize(query).distinct
  val length = qterms.length

  /**
   * Compute a score for a given document. Note that doc represents just one
   * single document and the elements in the list are just the words within the document. 
   */
  def score (doc: List[String]) : Double = {
    val tfs : Map[String,Int] = doc.groupBy(identity).mapValues(l => l.length)  // count absolute frequency of each word
    
    val querytfs = qterms.flatMap(q => tfs.get(q))
    val numTermsInCommon = querytfs.length 
    val docLen = tfs.values.map(x => x*x).sum.toDouble  // Euclidian norm
    val queryLen = qterms.length .toDouble  
    val termOverlap = querytfs.sum.toDouble / (docLen * queryLen)
    
    // top ordering is by terms in common (and semantics)
    // integer range from 0...qterms.length
    // on top of this a tf-based overlap score in range [0;1[ is added
    numTermsInCommon + termOverlap
  }
}