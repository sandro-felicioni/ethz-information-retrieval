package ch.ethz.dal.tinyir.io

import util.Try
import util.Failure
import util.Success
import io.Source
import java.io.File
import java.io.InputStream
import java.util.zip.ZipFile
import java.util.zip.ZipEntry
import scala.collection.JavaConversions._

/**
 * Create a document stream out of all files in all zip files
 * that are found in a given directory
 */
class ZipDirStream(dirpath: String, extension: String = "") extends DirStream(dirpath, extension) {

  /** The number of documents within all zip files */
  override def length: Int =
    ziplist.map(new ZipStream(_, extension).length).sum
    
  /** Creates a stream of inputStreams (i.e. one element in stream per document) */  
  override def stream: Stream[InputStream] =
    ziplist.map(new ZipStream(_, extension).stream).reduceLeft(_ append _)

  /** A sorted list containing all zip paths */
  val ziplist = new File(dirpath)
    .listFiles.filter(isZipFile(_))
    .map(z => z.getAbsolutePath).sorted.toList

  private def isZipFile(f: File) = f.getName.endsWith(".zip")
}

object ZipDirStream {
  def main(args: Array[String]) {
    val path = "/Users/thofmann/Data/Tipster/zips"
    val docs = new ZipDirStream(path)
    println("Reading from path = " + path)
    println("Number of documents = " + docs.length)
  }
}