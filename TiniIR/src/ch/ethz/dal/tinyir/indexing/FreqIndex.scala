package ch.ethz.dal.tinyir.indexing

import ch.ethz.dal.tinyir.processing.{Document,StringDocument}

case class FreqResult(val id: Int, val tf: List[Int]) extends Result[FreqResult] {
  def matches(that: FreqResult) = this.id compare that.id
  def matched(that: FreqResult) = FreqResult(id, this.tf ::: that.tf)
} 

class FreqIndex (docs: Stream[Document]) extends InvertedIndex[FreqResult] {

  case class FreqPosting(val id: Int, val freq: Int) extends Ordered[FreqPosting] {
    def compare(that: FreqPosting) = this.id compare that.id
  }
  
  type PostList = List[FreqPosting]
  
  val index : Map[String,PostList] = {
    val groupedTuples = postings(docs).groupBy(_.term)
    groupedTuples.mapValues(_.map(tfT => FreqPosting(tfT.doc, tfT.count)).sorted)
  }

  case class TfTuple(term: String, doc: Int, count: Int) 
  
  /** Create a list of triples (token, doc-id, frequency) for term frequencey */
  private def postings (s: Stream[Document]): List[TfTuple] =
    s.flatMap( doc => doc.tokens.groupBy(identity)
        .map{ case (token, list) => TfTuple(token, doc.ID, list.length) } ).toList
  
  override def results (term: String) : List[FreqResult] = 
    index.getOrElse(term,Nil).map(freqPosting => FreqResult(freqPosting.id, List(freqPosting.freq)))
}

object FreqIndex { 
  def main(args : Array[String]) = {
    // create tf index
    val d1 = new StringDocument(10,"mr sherlock holmes who was usually very late")
    val d0 = new StringDocument(12,"i said, i can tell a moriaty when i see one said holmes")  
    val stream : Stream[StringDocument] = List(d1,d0).toStream
    val idx = new FreqIndex(stream)    
    idx.index.foreach{ case (d,lst) => println(d + ": " + lst.mkString(" "))}     
    
    // create query and get top results
    println("\n\nquery results")
    val query = List("a","i")
    println(query.mkString(" ") + " = " + idx.results(query).mkString(" "))
  }
}
