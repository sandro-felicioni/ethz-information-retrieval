package ch.ethz.dal.classifier

import ch.ethz.dal.processing.ReutersCorpusIterator
import scala.collection
import ch.ethz.dal.processing.ReutersRCVParse
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ListBuffer
import com.github.aztek.porterstemmer.PorterStemmer
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import sun.awt.X11.XTextAreaPeer
import breeze.linalg.SparseVector
import breeze.linalg.VectorBuilder
import breeze.linalg.CSCMatrix
import scala.collection.immutable.Range

class LogisticRegressionClassifier(datasetPath: String, maxDocuments: Int, alpha: Int, removeStopwords: Boolean, useStemming: Boolean) extends AbstractClassifier {

  val stopwords = Set("", "a", "about", "above", "after", "again", "against", "all", "am", "an", "and", "any", "are", "aren't", "as", "at", "be", "because", "been", "before", "being", "below", "between", "both", "but", "by", "can't", "cannot", "could", "couldn't", "did", "didn't", "do", "does", "doesn't", "doing", "don't", "down", "during", "each", "few", "for", "from", "further", "had", "hadn't", "has", "hasn't", "have", "haven't", "having", "he", "he'd", "he'll", "he's", "her", "here", "here's", "hers", "herself", "him", "himself", "his", "how", "how's", "i", "i'd", "i'll", "i'm", "i've", "if", "in", "into", "is", "isn't", "it", "it's", "its", "itself", "let's", "me", "more", "most", "mustn't", "my", "myself", "no", "nor", "not", "of", "off", "on", "once", "only", "or", "other", "ought", "our", "ours	ourselves", "out", "over", "own", "same", "shan't", "she", "she'd", "she'll", "she's", "should", "shouldn't", "so", "some", "such", "than", "that", "that's", "the", "their", "theirs", "them", "themselves", "then", "there", "there's", "these", "they", "they'd", "they'll", "they're", "they've", "this", "those", "through", "to", "too", "under", "until", "up", "very", "was", "wasn't", "we", "we'd", "we'll", "we're", "we've", "were", "weren't", "what", "what's", "when", "when's", "where", "where's", "which", "while", "who", "who's", "whom", "why", "why's", "with", "won't", "would", "wouldn't", "you", "you'd", "you'll", "you're", "you've", "your", "yours", "yourself", "yourselves", "a's", "able", "about", "above", "according", "accordingly", "across", "actually", "after", "afterwards", "again", "against", "ain't", "all", "allow", "allows", "almost", "alone", "along", "already", "also", "although", "always", "am", "among", "amongst", "an", "and", "another", "any", "anybody", "anyhow", "anyone", "anything", "anyway", "anyways", "anywhere", "apart", "appear", "appreciate", "appropriate", "are", "aren't", "around", "as", "aside", "ask", "asking", "associated", "at", "available", "away", "awfully", "be", "became", "because", "become", "becomes", "becoming", "been", "before", "beforehand", "behind", "being", "believe", "below", "beside", "besides", "best", "better", "between", "beyond", "both", "brief", "but", "by", "c'mon", "c's", "came", "can", "can't", "cannot", "cant", "cause", "causes", "certain", "certainly", "changes", "clearly", "co", "com", "come", "comes", "concerning", "consequently", "consider", "considering", "contain", "containing", "contains", "corresponding", "could", "couldn't", "course", "currently", "definitely", "described", "despite", "did", "didn't", "different", "do", "does", "doesn't", "doing", "don't", "done", "down", "downwards", "during", "each", "edu", "eg", "eight", "either", "else", "elsewhere", "enough", "entirely", "especially", "et", "etc", "even", "ever", "every", "everybody", "everyone", "everything", "everywhere", "ex", "exactly", "example", "except", "far", "few", "fifth", "first", "five", "followed", "following", "follows", "for", "former", "formerly", "forth", "four", "from", "further", "furthermore", "get", "gets", "getting", "given", "gives", "go", "goes", "going", "gone", "got", "gotten", "greetings", "had", "hadn't", "happens", "hardly", "has", "hasn't", "have", "haven't", "having", "he", "he's", "hello", "help", "hence", "her", "here", "here's", "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "hi", "him", "himself", "his", "hither", "hopefully", "how", "howbeit", "however", "i'd", "i'll", "i'm", "i've", "ie", "if", "ignored", "immediate", "in", "inasmuch", "inc", "indeed", "indicate", "indicated", "indicates", "inner", "insofar", "instead", "into", "inward", "is", "isn't", "it", "it'd", "it'll", "it's", "its", "itself", "just", "keep", "keeps", "kept", "know", "known", "knows", "last", "lately", "later", "latter", "latterly", "least", "less", "lest", "let", "let's", "like", "liked", "likely", "little", "look", "looking", "looks", "ltd", "mainly", "many", "may", "maybe", "me", "mean", "meanwhile", "merely", "might", "more", "moreover", "most", "mostly", "much", "must", "my", "myself", "name", "namely", "nd", "near", "nearly", "necessary", "need", "needs", "neither", "never", "nevertheless", "new", "next", "nine", "no", "nobody", "non", "none", "noone", "nor", "normally", "not", "nothing", "novel", "now", "nowhere", "obviously", "of", "off", "often", "oh", "ok", "okay", "old", "on", "once", "one", "ones", "only", "onto", "or", "other", "others", "otherwise", "ought", "our", "ours", "ourselves", "out", "outside", "over", "overall", "own", "particular", "particularly", "per", "perhaps", "placed", "please", "plus", "possible", "presumably", "probably", "provides", "que", "quite", "qv", "rather", "rd", "re", "really", "reasonably", "regarding", "regardless", "regards", "relatively", "respectively", "right", "said", "same", "saw", "say", "saying", "says", "second", "secondly", "see", "seeing", "seem", "seemed", "seeming", "seems", "seen", "self", "selves", "sensible", "sent", "serious", "seriously", "seven", "several", "shall", "she", "should", "shouldn't", "since", "six", "so", "some", "somebody", "somehow", "someone", "something", "sometime", "sometimes", "somewhat", "somewhere", "soon", "sorry", "specified", "specify", "specifying", "still", "sub", "such", "sup", "sure", "t's", "take", "taken", "tell", "tends", "th", "than", "thank", "thanks", "thanx", "that", "that's", "thats", "the", "their", "theirs", "them", "themselves", "then", "thence", "there", "there's", "thereafter", "thereby", "therefore", "therein", "theres", "thereupon", "these", "they", "they'd", "they'll", "they're", "they've", "think", "third", "this", "thorough", "thoroughly", "those", "though", "three", "through", "throughout", "thru", "thus", "to", "together", "too", "took", "toward", "towards", "tried", "tries", "truly", "try", "trying", "twice", "two", "un", "under", "unfortunately", "unless", "unlikely", "until", "unto", "up", "upon", "us", "use", "used", "useful", "uses", "using", "usually", "value", "various", "very", "via", "viz", "vs", "want", "wants", "was", "wasn't", "way", "we", "we'd", "we'll", "we're", "we've", "welcome", "well", "went", "were", "weren't", "what", "what's", "whatever", "when", "whence", "whenever", "where", "where's", "whereafter", "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "which", "while", "whither", "who", "who's", "whoever", "whole", "whom", "whose", "why", "will", "willing", "wish", "with", "within", "without", "won't", "wonder", "would", "wouldn't", "yes", "yet", "you", "you'd", "you'll", "you're", "you've", "your", "yours", "yourself", "yourselves", "zero")

  /* All topics that are available */
  var topics = Map[String, Int]()

  /* the vocabulary that is used */
  var vocabulary = Map[String, Int]()

  var numTrainingSamples: Int = 0

  /* each column is a weight vector w for a topic that is available  */
  var weightVectors: DenseMatrix[Double] = null

  /* kxn matrix where k = number of features and n = number of samples*/
  var X_train: CSCMatrix[Double] = null

  /* lxn matrix where l = number of topics/labels and n = number of samples */
  var Y_train: DenseMatrix[Double] = null

  private def predictTopicsForDocument(doc: ReutersRCVParse): Set[String] = {
    return Set[String]()
  }

  private def getCleanTokens(tokens: List[String]): List[String] = {
    var cleanTokens = tokens.map(token => token.toLowerCase())
    if (removeStopwords) {
      cleanTokens = cleanTokens.filter(token => token.length() > 2 && !stopwords.contains(token))
    }
    if (useStemming) {
      cleanTokens = cleanTokens.map(token => PorterStemmer.stem(token))
    }
    return cleanTokens
  }

  private def retrieveTopicsAndVocabulary(): Unit = {
    // retrieve all topics and fix the vocabulary
    var tempTopics = collection.mutable.Set[String]()
    var tempVocabulary = collection.mutable.Set[String]()

    val documentIterator = new ReutersCorpusIterator(datasetPath)
    for (doc <- documentIterator) {
      tempTopics ++= doc.topics
      tempVocabulary ++= doc.tokens
      numTrainingSamples += 1
    }
    vocabulary = tempVocabulary.zipWithIndex.map({ case (term, index) => term -> index }).toMap
    topics = tempTopics.zipWithIndex.map({ case (topic, index) => topic -> index }).toMap

    println("num samples: " + numTrainingSamples)
    println("num topics: " + topics.size)
    println("dictionary size: " + vocabulary.size)
    println("topics retrieved and vocabulary size fixed")
  }

  private def extractTrainingData(): Unit = {
    var doc_idx = 0
    Y_train = DenseMatrix.fill(rows = topics.size, cols = numTrainingSamples)(-1)
    val matrixBuilder = new CSCMatrix.Builder[Double](rows = vocabulary.size, cols = numTrainingSamples)
    val documentIterator = new ReutersCorpusIterator(datasetPath)
    for (doc <- documentIterator) {
      
      // extract features
      var tf = doc.tokens.groupBy(identity).mapValues(valueList => valueList.length)
      for ((term, frequency) <- tf) {
        val term_idx = vocabulary.getOrElse(term, -1) // index of term in dictionary or -1 if not present
        matrixBuilder.add(term_idx, doc_idx, frequency)
      }

      // extract labels
      for (topic <- doc.topics) {
        val topic_idx = topics.get(topic).get
        Y_train(topic_idx, doc_idx) = 1
      }
      doc_idx += 1
    }
    
    // create spare matrix in order to not waste memory
    X_train = matrixBuilder.result
  }

  def training() = {
    retrieveTopicsAndVocabulary()
    extractTrainingData()
    
    
    // initialize weight vectors
    // weightVectors = DenseMatrix.zeros[Double](X_train.cols, topics.size)

    // stop words removal
    // smoothing? hard to implement..
    // stemming
  }

  def predict(testsetPath: String): Map[Int, Set[String]] = {
    var documentClassifications = Map[Int, Set[String]]()
    val documentIterator = new ReutersCorpusIterator(testsetPath)
    for (doc <- documentIterator) {
      val documentClassification = predictTopicsForDocument(doc)
      documentClassifications += ((doc.ID, documentClassification))
    }

    return documentClassifications
  }
}