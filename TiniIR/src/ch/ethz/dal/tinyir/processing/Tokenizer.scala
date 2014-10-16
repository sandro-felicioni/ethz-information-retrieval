package ch.ethz.dal.tinyir.processing

/**
 * This tokenizer is used to split a query in it's single terms
 */
object Tokenizer {
  def tokenize (text: String) : List[String] =
    text.split("[ .,;:?!\t\n\r\f]+").toList
}