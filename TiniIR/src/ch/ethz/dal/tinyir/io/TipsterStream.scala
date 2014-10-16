

package ch.ethz.dal.tinyir.io

import ch.ethz.dal.tinyir.processing.XMLDocument
import ch.ethz.dal.tinyir.processing.TipsterParse

/**
 * This is the main class, which should be used to parse all documents.
 */
class TipsterStream (datasetPath: String, ext: String = "") extends ParsedXMLStream(new ZipDirStream(datasetPath, "")){
  
  /** Get a stream of XMLDocument's from which you can easily read title, content etc. */
  def stream : Stream[XMLDocument] = unparsed.stream.map(is => new TipsterParse(is))
  
  /** The number of documents to parse */
  def length = unparsed.length 
}

object TipsterStream  {

  def main(args: Array[String]) {
    val tipster = new TipsterStream ("./tipster-dataset/zips")  
    println("Number of files in zips = " + tipster.length)
    
    var length : Long = 0 
    var tokens : Long = 0
    for (doc <- tipster.stream.take(10000)) { 
      length += doc.content.length
      tokens += doc.tokens.length
    }
    println("Final number of characters = " + length)
    println("Final number of tokens     = " + tokens)
  }
}