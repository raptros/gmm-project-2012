package getcells
import scala.collection.immutable.{Stream, PagedSeq}
import scala.util.parsing.combinator._
import scala.util.parsing.input.PagedSeqReader

import java.io.{Reader, File, FileReader, FileWriter, PrintWriter}

/** finds cells for documents*/
class Locator(val cells:Set[SquareCell]) {

  def findCell(doc:DocWithLoc):Option[Int] = cells.zipWithIndex.filter {
    pair => pair._1.contains(doc.geoTag)
  }.headOption.map(p => p._2)
}

/** A crude cli app that matches up docs with cells.*/
@EnhanceStrings
object GetCells extends App {
  val logFile = args(0)
  val docsFile = args(1)
  val cellsFile = args(2)
  val tagsFile = args(3)
  val cellOut = new PrintWriter(new FileWriter(new File(cellsFile)))
  val tagOut = new PrintWriter(new FileWriter(new File(tagsFile)))
  println("reading cells from #logFile; docs from #docsFile")
  val cells = LogParser.loadLogCells(logFile)
  println("writing cell bounds to #cellsFile")
  cells.zipWithIndex foreach { pair =>
    val (cell, id) = pair
    cellOut.println("\"G#id\", \"#cell\"")
  }
  cellOut.close()
  println("writing doc locations to #tagsFile")
  val locator = new Locator(cells)
  val docs = CoordsParser.loadDocs(docsFile)
  docs foreach { doc =>
    locator.findCell(doc) foreach { cell => 
      tagOut.println("\"#{doc.docId}\", \"G#cell\"")
    }
  }
  tagOut.close()
  println("done")
}
