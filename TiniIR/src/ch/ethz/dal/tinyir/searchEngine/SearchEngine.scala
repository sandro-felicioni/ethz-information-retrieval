package ch.ethz.dal.tinyir.searchEngine

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.util.StopWatch
import scala.io.Source
import ch.ethz.dal.tinyir.lectures.TipsterGroundTruth

object SearchEngine {

  /** Get the queries along with their id => List of tuples in format (query, query-id) */
  def getQueries(filePath: String): List[(String, Int)] = {
    var queryIterator = Source.fromFile("./tipster-dataset/topics").getLines
    var queries = queryIterator.filter(line => line.startsWith("<title>")).map(line => line.substring(line.indexOf("Topic:") + "Topic:".length()).trim()).toList

    var IdIterator = Source.fromFile("./tipster-dataset/topics").getLines
    var query_ids = IdIterator.filter(line => line.startsWith("<num>")).map(line => line.substring(line.indexOf("Number:") + "Number:".length()).trim().toInt).toList

    return queries.zip(query_ids)
  }

  def printQueryResults(queryId: Int, queryResults: List[(String, Double)]) = {
    var rank = 1
    for ((documentId, score) <- queryResults) {
      println(queryId + " " + rank + " " + documentId + " " + score)
      rank += 1
    }
  }

  def printPrecision(queryId: Int, relevantDocuments: Set[String], retrievedDocuments: Set[String]) = {
    if (retrievedDocuments.size > 0) {
      println(queryId + "\tRetrieved Documents: " + retrievedDocuments.size + "\tPrecision: " + relevantDocuments.intersect(retrievedDocuments).size / retrievedDocuments.size.toDouble)
    }
  }

  def main(args: Array[String]) {
    var watch = new StopWatch()
    watch.start

    // create model
    var model = new TermModel(new TipsterStream("./tipster-dataset/zips"))
    model.computeModel()

    // process queries
    val queries = getQueries("./tipster-dataset/topics").take(2)
    val maxDocuments = 100 // max number of documents per query that are returned
    val results = model.computeScore(queries, maxDocuments)

    // validate with ground truth
    val groundTruth = new TipsterGroundTruth("./tipster-dataset/qrels")
    for ((queryId, queryResults) <- results) {
      printQueryResults(queryId, queryResults)

      //ground truth is available only for training data (i.e. queryId's 50-90)
      if (queryId <= 90) {
        val relevantDocuments = groundTruth.judgements.get(queryId.toString).get.toSet
        val retrievedDocuments = queryResults.map({ case (docId, score) => docId }).toSet
        printPrecision(queryId, relevantDocuments, retrievedDocuments)
      }
    }

    watch.stop
    println("time = " + watch.stopped)
  }
}