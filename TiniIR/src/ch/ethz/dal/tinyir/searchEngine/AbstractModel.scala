package ch.ethz.dal.tinyir.searchEngine

import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.processing.XMLDocument

abstract class AbstractModel {

  /** Create a list of triples (token, doc-id, relative-frequency-in-document) for term frequency within a document */
  private def createFrequencyTriples(stream: Stream[XMLDocument]): List[(String, String, Double)] = {
    return stream.flatMap(doc => doc.tokens.groupBy(identity)
      .map { case (token, list) => (token, doc.name, list.length / doc.tokens.size.toDouble) })
      .toList
  }
  
  /** Create a list of quadruples (token, doc-id, absolute-frequency-in-document, document-size) for term frequency within a document */
  private def createFrequencyQuadruple(stream: Stream[XMLDocument]): List[(String, String, Int, Int)] = {
    return stream.flatMap(doc => doc.tokens.groupBy(identity)
      .map { case (token, list) => (token, doc.name, list.length,  doc.tokens.size) })
      .toList
  }  
  
  /** Create a relative term frequency model */
  protected def computeRelativeTermFrequencies(stream: Stream[XMLDocument]): Map[String, List[(String, Double)]] = {
    return createFrequencyTriples(stream)
      .groupBy(triple => triple._1)
      .mapValues(list => list.map(triple => (triple._2, triple._3)));
  }
  
  /** Create an absolute term frequency model */
  protected def computeAbsoluteTermFrequencies(stream: Stream[XMLDocument]): Map[String, List[(String, Int, Int)]] = {
    return createFrequencyQuadruple(stream)
      .groupBy(quadruple => quadruple._1)
      .mapValues(list => list.map(quadruple => (quadruple._2, quadruple._3, quadruple._4)));
  }
  
  /** Compute log_2 */
  protected def log2(x: Double) : Double = {
    return math.log(x) / math.log(2)
  }
  
  def computeModel(stream: Stream[XMLDocument])
  def computeScore(query: String) : List[String]
}