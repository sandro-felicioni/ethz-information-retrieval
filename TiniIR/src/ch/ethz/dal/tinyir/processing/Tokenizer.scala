package ch.ethz.dal.tinyir.processing

/**
 * This tokenizer is used to split a query in it's single terms
 */
object Tokenizer {
  
  /** Split strings in a text */
  def tokenize (text: String) : List[String] =
    text.split("[ .,;:?!\t\n\r\f]+").toList
    
  /** Transform a list of strings into lower case strings */
  def toLowerCase(list: List[String]) = {
    list.map(token => token.toLowerCase())
  }
  
  private def lower(character: Char): Char = {
    if (character.isLetter)
      return character.toLower
    return character
  }
}