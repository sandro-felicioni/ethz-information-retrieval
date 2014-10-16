package ch.ethz.dal.tinyir.searchEngine

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.Tokenizer
import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.util.StopWatch

/**
 * SearchEngine can be used to first parse all documents and hereby create a model (e.g. tf-idf)
 * which then later can be used to retrieve relevant documents.
 */
class SearchEngine(datasetPath: String) {
  val tipster = new TipsterStream(datasetPath)
  var termModel = new TermModel()

  def computeTFIDFModel() = {
    termModel.computeTFIDFModel(tipster.stream)
  }

  def scoreTFIDF(query: String): List[String] = {
    return termModel.scoreTFIDF(query)
  }
}

object SearchEngine {
  def main(args: Array[String]) {
    val path = "./tipster-dataset/zips"
    var searchEngine = new SearchEngine(path)

    var watch = new StopWatch()
    watch.start
    searchEngine.computeTFIDFModel()
    var relevantDocuments = searchEngine.scoreTFIDF("Airbus Subsidies")
    watch.stop

    println("time = " + watch.stopped)
  }
}