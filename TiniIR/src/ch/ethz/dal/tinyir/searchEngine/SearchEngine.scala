package ch.ethz.dal.tinyir.searchEngine

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.util.StopWatch
import scala.io.Source

object SearchEngine {
  
  /** Get the queries along with their id => List of tuples in format (query, query-id) */
  def getQueries(filePath: String) : List[(String, Int)] = {
    var queryIterator = Source.fromFile("./tipster-dataset/topics").getLines
    var queries = queryIterator.filter(line => line.startsWith("<title>")).map(line => line.substring(line.indexOf("Topic:")+"Topic:".length()).trim()).toList
    
    var IdIterator = Source.fromFile("./tipster-dataset/topics").getLines
    var query_ids = IdIterator.filter(line => line.startsWith("<num>")).map(line => line.substring(line.indexOf("Number:")+"Number:".length()).trim().toInt).toList

    return queries.zip(query_ids)
  }
  
  def printQueryResults(queryId: Int, scores: List[(String, Int)]) = {
    for( (documentId, rank) <- scores){
      println(queryId + " " + rank + " " + documentId)
    }
  }
  
  def main(args: Array[String]) {
//    var watch = new StopWatch()
//    watch.start

    // create model
    val tipster = new TipsterStream("./tipster-dataset/zips")
    var model = new LanguageModel()
    model.computeModel(tipster.stream.take(500))
    
    // process queries
    val queries = getQueries("./tipster-dataset/topics")
    for( (query, queryId) <- queries.take(5)){
	    val scores = model.computeScore(query).zip(Stream from 1)
	    printQueryResults(queryId, scores)
    }
    
    
    
//    watch.stop
//    println("time = " + watch.stopped)
  }
}