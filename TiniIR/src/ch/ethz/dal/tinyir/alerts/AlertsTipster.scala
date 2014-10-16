package ch.ethz.dal.tinyir.alerts

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.util.StopWatch
import ch.ethz.dal.tinyir.lectures.TipsterGroundTruth
import ch.ethz.dal.tinyir.lectures.PrecisionRecall

class AlertsTipster(query: String, num: Int) extends Alerts(query, num) 

object AlertsTipster {

  /**
   * Compute the top 100 documents which are relevant for a given query. 
   * Note that for a single query the entire corpus is parsed.
   */
  def main(args: Array[String]) {  

    // find the top 100 documents for the query "Airbus Subsidies" 
    val query = "Airbus Subsidies"
    val num = 100
    val alerts = new AlertsTipster(query, num)
    val tipster = new TipsterStream("./tipster-dataset/zips")

    val sw = new StopWatch; sw.start
    var iter = 0
    for (doc <- tipster.stream.take(30000)) {
      iter += 1
      alerts.process(doc.name, doc.tokens)
      if (iter % 20000 ==0) {
        println("Iteration = " + iter)
        alerts.results.foreach(println)    
      }  
    }
    sw.stop
    println("Stopped time = " + sw.stopped) 
    alerts.results.foreach(println)  
    
    // print precision/recall metric for the query
    val relevantDocuments = new TipsterGroundTruth("./tipster-dataset/qrels").judgements.get("51").get.toSet
    val ret = alerts.results.map(r => r.title)
    val pr = new PrecisionRecall(ret,relevantDocuments)
    println(pr.relevIdx.mkString(" "))
    println(pr.precs.mkString(" "))
    println(pr.iprecs.mkString(" "))
  }
  
}