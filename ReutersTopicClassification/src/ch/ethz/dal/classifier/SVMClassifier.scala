package ch.ethz.dal.classifier

import scala.collection
import scala.collection.immutable.Range
import scala.util.Random
import com.github.aztek.porterstemmer.PorterStemmer
import breeze.linalg.CSCMatrix
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.SparseVector
import ch.ethz.dal.processing.ReutersCorpusIterator
import ch.ethz.dal.processing.ReutersRCVParse
import breeze.linalg.VectorBuilder
import scala.collection.mutable.ListBuffer
import breeze.linalg.StorageVector
import scala.collection.parallel.mutable.ParHashMap

class SVMClassifier(datasetPath: String, restrictedTopics: Set[String], lambda: Double, removeStopwords: Boolean, useStemming: Boolean) extends AbstractClassifier(restrictedTopics, removeStopwords, useStemming) {

  /** A map which stores the matrix indices for each topic */
  var topics = Map[String, Int]()

  var vocabulary = Map[String, Int]()

  /** total number of documents that are used to build the vocabulary and topic set*/
  var numDocuments: Int = 0

  /** the number of effective training set size that is used during training */
  var numTrainingSamples: Int = 0

  var numFeatures: Int = 0

  /** each entry corresponds to a topic_index and a weight vector w */
  var weightVectors = new ParHashMap[Int, SparseVector[Double]]()

  /** this map stores for each document a sparse vector */
  var X_train = new collection.mutable.HashMap[Int, SparseVector[Double]]()

  /** lxn matrix where l = number of topics/labels and n = number of samples */
  var Y_train: DenseMatrix[Int] = null

  def training() = {
    retrieveTopicsAndVocabulary()
    extractTrainingData()

    // train
    print("Training model:")
    Range(0, topics.size).par.foreach(topic_idx => trainTopic(topic_idx))
    print("done\n")
  }

  private def retrieveTopicsAndVocabulary(): Unit = {
    // retrieve all topics and fix the vocabulary
    var tempTopics = collection.mutable.Set[String]()
    var vocabularyFrequency = collection.mutable.Map[String, Int]()

    val documentIterator = new ReutersCorpusIterator(datasetPath)
    for (doc <- documentIterator.take(50000)) {
      tempTopics ++= doc.topics
      val cleanTokens = getCleanTokens(doc.tokens)
      vocabularyFrequency ++= cleanTokens.groupBy(identity).mapValues(list => list.length).map { case (term, frequency) => term -> (frequency + vocabularyFrequency.getOrElse(term, 0)) }

      numDocuments += 1
    }

    // printFrequencies(vocabularyFrequency)
    dropLowFrequencyTerms(vocabularyFrequency, lowerThreshold = 12)

    vocabulary = vocabularyFrequency.keys.zipWithIndex.map({ case (term, index) => term -> index }).toMap
    topics = filterTopics(tempTopics.toSet).zipWithIndex.map({ case (topic, index) => topic -> index }).toMap
    println(topics)

    println("num documents: " + numDocuments)
    println("num topics: " + topics.size)
    println("dictionary size: " + vocabulary.size)
    println("topics retrieved and vocabulary size fixed")
  }

  /** Drop terms that have a frequency <= lowerThreshold */
  private def dropLowFrequencyTerms(vocabularyFrequency: collection.mutable.Map[String, Int], lowerThreshold: Int): Unit = {
    val before = vocabularyFrequency.size
    vocabularyFrequency.retain { case (term, frequency) => frequency > lowerThreshold }
    val after = vocabularyFrequency.size
    println("dropped " + (before - after) + " low frequency terms from vocabulary! New Size = " + after)
  }

  /** Drop terms that have a frequency >= upperThreshold */
  private def dropHighFrequencyTerms(vocabularyFrequency: collection.mutable.Map[String, Int], upperThreshold: Int): Unit = {
    val before = vocabularyFrequency.size
    vocabularyFrequency.retain { case (term, frequency) => frequency < upperThreshold }
    val after = vocabularyFrequency.size
    println("dropped " + (before - after) + " high frequency terms from vocabulary! New Size = " + after)
  }

  private def printFrequencies(termFrequencies: collection.mutable.Map[String, Int]): Unit = {
    termFrequencies.valuesIterator.foreach(frequency => print(frequency + ","))
    println("")
  }

  private def extractTrainingData(): Unit = {

    // pre-allocate maximal label matrix
    Y_train = DenseMatrix.fill(rows = topics.size, cols = numDocuments)(-1) // -1 => has not label and 1 => has label
    numFeatures = vocabulary.size + 1

    var doc_idx = 0
    val documentIterator = new ReutersCorpusIterator(datasetPath)
    for (doc <- documentIterator.take(50000)) {

      // extract features
      X_train += (doc_idx -> extractFeaturesForDocument(doc).toSparseVector)

      // extract labels
      for (topic <- filterTopics(doc.topics)) {
        val topic_idx = topics.get(topic).get
        Y_train(topic_idx, doc_idx) = 1
      }
      doc_idx += 1
    }
    numTrainingSamples = doc_idx

    // assign Y the correct dimensions of the label matrix
    Y_train = Y_train(::, 0 to numTrainingSamples - 1)

    println("Training data extracted - numSamples = " + numTrainingSamples + " numFeatures = " + numFeatures)
  }

  private def extractFeaturesForDocument(doc: ReutersRCVParse): VectorBuilder[Double] = {
    var tf = getCleanTokens(doc.tokens).groupBy(identity).mapValues(valueList => valueList.length)
    val vectorBuilder = new VectorBuilder[Double](numFeatures)
    for ((term, frequency) <- tf) {
      val term_idx = vocabulary.getOrElse(term, -1) // index of term in dictionary or -1 if not present
      if (term_idx != -1) {
        vectorBuilder.add(term_idx, frequency)
      }
    }

    // add intercept as last element
    vectorBuilder.add(vectorBuilder.size - 1, 1)
    return vectorBuilder
  }

  private def trainTopic(topic_idx: Int): Unit = {
    var w = SparseVector.zeros[Double](numFeatures)
    var eta: Double = 1

    // compute weights due to imbalanced data
    val numPositive = (Y_train(topic_idx, ::).t :== 1).activeSize
    val numNegative = (Y_train(topic_idx, ::).t :== -1).activeSize
    val alphaPlus = numPositive / numTrainingSamples.toDouble
    val alphaMinus = numNegative / numTrainingSamples.toDouble

    // perform SGD to train the model
    var generator = new Random(topic_idx)
    for (t <- Range(1, 20000)) {
      val idx = generator.nextInt(numTrainingSamples)
      val x = X_train.get(idx).get
      val y = Y_train(topic_idx, idx)
      w = w - gradient(w, x, y, alphaPlus, alphaMinus) * (eta / (lambda * t))

      //      if ( (t+1) % 5000 == 0){
      //        testTrainingError(topic_idx, w)
      //      }
    }

    weightVectors += (topic_idx -> w)
    print(weightVectors.size + " ")
  }

  /**
   *  Computes the gradient of the negative log-logistic loss function: l(w; x, y) = 1 + exp(-y * w^x)
   *  Note that due to the limits of breeze we always have to compute vector * scalar. Vice versa doesn't compile!
   *  Note also that the this method expects the label y to be element of {-1, 1}!
   */
  private def gradient(w: SparseVector[Double], x: SparseVector[Double], y: Double, alphaPlus: Double, alphaMinus: Double): SparseVector[Double] = {
    var alpha = 1d
    if (y == 1) {
      // max required for the unlikely case where we would have more positive than negative samples
      alpha = math.max(alpha, 1 / alphaPlus - 1)
    }

    if (y * (w dot x) >= alpha) {
      return w * lambda
    }
    return w * lambda - x * y
  }

  /**
   * Classifies the sample x with weight vector w.
   */
  private def classify(w: StorageVector[Double], x: SparseVector[Double]): Double = {
    return math.signum(w.dot(x))
  }

  /**
   * A help method to test whether we improve on the training set during the learning process.
   * If not, then it is an indicator that something is wrong!
   */
  private def testTrainingError(topic_idx: Int, w: SparseVector[Double]) {
    var truePositive = 0
    var trueNegative = 0
    var falsePositive = 0
    var falseNegative = 0
    var numPositives = 0
    var numNegatives = 0

    for (i <- Range(0, numTrainingSamples)) {
      val x = X_train.get(i).get
      val y = Y_train(topic_idx, i)
      val y_hat = if (classify(w, x) >= 0) 1 else -1

      if (y == 1 && y_hat == 1) {
        numPositives += 1
        truePositive += 1
      } else if (y == 1 && y_hat == -1) {
        numPositives += 1
        falseNegative += 1
      } else if (y == -1 && y_hat == -1) {
        numNegatives += 1
        trueNegative += 1
      } else if (y == -1 && y_hat == 1) {
        numNegatives += 1
        falsePositive += 1
      }
    }
    val TPRate = truePositive / numPositives.toDouble
    val TNRate = trueNegative / numNegatives.toDouble

    println("Topic: " + topic_idx + " TP rate = " + TPRate + " (" + truePositive + "/" + numPositives + ") TN rate = " + TNRate + " (" + trueNegative + "/" + numNegatives + ")")
  }

  def predict(testsetPath: String): Map[Int, Set[String]] = {
    var documentClassifications = Map[Int, Set[String]]()
    val documentIterator = new ReutersCorpusIterator(testsetPath)
    for ((doc, count) <- documentIterator.zipWithIndex) {
      val documentClassification = predictTopicsForDocument(doc)
      documentClassifications += ((doc.ID, documentClassification))

      if ((count + 1) % 10000 == 0) {
        println("classified: " + (count + 1))
      }
    }

    return documentClassifications
  }

  private def predictTopicsForDocument(doc: ReutersRCVParse): Set[String] = {
    var classifiedTopcis = new ListBuffer[String]()
    var x = extractFeaturesForDocument(doc).toSparseVector
    for (topic <- topics.keys) {
      val topic_idx = topics.get(topic).get
      var w = weightVectors.get(topic_idx).get

      if (classify(w, x) >= 0) {
        classifiedTopcis += topic
      }
    }
    return classifiedTopcis.toSet
  }
}