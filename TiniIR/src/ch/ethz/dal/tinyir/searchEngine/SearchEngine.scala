package ch.ethz.dal.tinyir.searchEngine

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.util.StopWatch
import scala.io.Source
import ch.ethz.dal.tinyir.lectures.TipsterGroundTruth
import java.io.PrintWriter
import java.io.FileOutputStream
import java.util.Date
import ch.ethz.dal.tinyir.lectures.PrecisionRecall
import ch.ethz.dal.tinyir.lectures.AveragePrecision

object SearchEngine {

  /** Get the queries along with their id => List of tuples in format (query, query-id) */
  def getQueries(filePath: String): List[(String, Int)] = {
    var queryIterator = Source.fromFile(filePath).getLines
    var queries = queryIterator.filter(line => line.startsWith("<title>")).map(line => line.substring(line.indexOf("Topic:") + "Topic:".length()).trim()).toList

    var IdIterator = Source.fromFile(filePath).getLines
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
  
  def writeQueryResults(queryId: Int, queryResults: List[(String, Double)], writer: PrintWriter) = {
    var rank = 1
    for ((documentId, score) <- queryResults) {
      writer.println(queryId + " " + rank + " " + documentId)
      rank += 1
    }
    writer.flush()
  }

  def main(args: Array[String]) {
    // read out parameters
    val rootPath = "/Users/Sandro/projects/ethz-information-retrieval/TiniIR/tipster-dataset/"
    var pathToDataset = rootPath + "zips/"
    var pathToQueries = rootPath + "topics"
    var pathToGroundTruth = rootPath + "qrels"
    var termModel = true
    if (args.length == 4){
      pathToDataset = args(0)
	  pathToQueries = args(1)
	  pathToGroundTruth = args(2)
	  if (args(3) == "LM")
	    termModel = false
    }else{
      println("Not enough arguments (default values are now used), provide exactly 4 arguments:")
      println("\tpath to dataset which contains all zips: Default value = " + pathToDataset)
      println("\tpath to queries: Default value = " + pathToQueries)
      println("\tpath to ground truth values: Default value = " + pathToGroundTruth)
      println("\tThe model you want to use (either TM or LM): Default value = " + "TM")
    }

    
    var watch = new StopWatch()
    watch.start
    
    // create model
    var model : AbstractModel = null
    if (termModel)
      model = new TermModel(new TipsterStream(pathToDataset), 30000)
    else
      model = new LanguageModel(new TipsterStream(pathToDataset), 30000)
    model.computeModel()

    // process queries
    val queries = getQueries(pathToQueries)
    val maxDocuments = 100 // max number of documents per query that are returned
    val results = model.computeScore(queries, maxDocuments)

    // validate with ground truth
    val groundTruth = new TipsterGroundTruth(pathToGroundTruth)
    var MAP = new collection.mutable.ListBuffer[(Int, Double)]()
    for ((queryId, queryResults) <- results) {
      printQueryResults(queryId, queryResults)

      //ground truth is available only for training data (i.e. queryId's 50-90)
      if (queryId <= 90) {
        val relevantDocuments = groundTruth.judgements.get(queryId.toString).get.toSet
        val retrievedDocuments = queryResults.map({ case (docId, score) => docId }).toList
        val ap = new AveragePrecision(retrievedDocuments, relevantDocuments, 100).avgPrecision
        MAP.append((queryId, ap))
      }
    }
    MAP.foreach(ap => println(ap._1 + "\tAP =  " + ap._2) )
    println("MAP = " + MAP.map(_._2).sum / MAP.length.toDouble)
    
    // write results to file for queries 91-100 (test data)
    val usedModel = if (termModel) "t" else "l"
    val writer = new PrintWriter(new FileOutputStream("ranking-" + usedModel + "-sandro-felicioni-" + new Date() + ".run"))
    for ((queryId, queryResults) <- results if queryId > 90) {
    	writeQueryResults(queryId, queryResults, writer)
    }
    println("Files written to disk")
    
    watch.stop
    println("time = " + watch.stopped)
  }
}