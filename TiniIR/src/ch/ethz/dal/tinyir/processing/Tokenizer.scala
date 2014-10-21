package ch.ethz.dal.tinyir.processing

/**
 * This tokenizer is used to split a query in it's single terms
 */
object Tokenizer {
  
  /** Split strings in a text */
  def tokenize (text: String) : List[String] =
    text.split("[ _.,;:?!`\'\"\t\n\r\f]+").toList
    
  /** Transform a list of strings into lower case strings */
  def toLowerCase(list: List[String]) = {
    list.map(token => token.toLowerCase())
  }
  
  def main(args: Array[String]){
	val s1 = "I, have a question?"
    println(s1 + " => " + tokenize(s1))
    assert(tokenize(s1)(0) == "I")
    assert(tokenize(s1)(3) == "question")

    val s2 = "My name is '`marty\"."
    println(s2 + " => " + tokenize(s2))
    assert(tokenize(s2)(3) == "marty")
  }
}