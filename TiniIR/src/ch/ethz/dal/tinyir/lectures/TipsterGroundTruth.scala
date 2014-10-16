package ch.ethz.dal.tinyir.lectures

import scala.io.Source
import scala.collection.immutable.Map
import scala.Array.canBuildFrom

/**
 * This class can be used for the evaluation part. It reads out the ground truth values of the relevant documents. 
 * In "judgments" are stored only those documents which are relevant for a given topic. Hence when it comes to
 * computing the precision/recall score all documents which are not listed for a certain topic are considered irrelevant! 
 */
class TipsterGroundTruth(path:String) {
  
  /** A map consisting of <String, String[]> where the key is the topic-id and the values are all relevant documents */
  val judgements: Map[String, Array[String]] =
	  Source.fromFile(path).getLines()
	  .filter(l => !l.endsWith("0"))
	  .map(l => l.split(" "))
	  .map(e => (e(0), e(2)))
	  .toArray
	  .groupBy(_._1)
	  .mapValues(_.map(_._2))

}

object TipsterGroundTruth {
  
  def main (args:Array[String]){
    val t = new TipsterGroundTruth("./tipster-dataset/qrels")
    t.judgements.toList.sortBy(_._1).foreach(j => println("Topic "+j._1 +": "+j._2.size+" judgements found"))
  }
}