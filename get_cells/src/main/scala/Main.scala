package getcells
import scala.collection.immutable.{Stream, PagedSeq}
import scala.util.parsing.combinator._
import scala.util.parsing.input.PagedSeqReader

import java.io.{Reader, File, FileReader}

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
  println("logFile: #logFile; docsFile: #docsFile")
  val cells = LogParser.loadLogCells(logFile)
  cells.zipWithIndex foreach { pair =>
    val (cell, id) = pair
    println("cell #id: #cell")
  }
  println()
  val locator = new Locator(cells)
  val docs = CoordsParser.loadDocs(docsFile)
  docs foreach { doc =>
    val cell = locator.findCell(doc)
    println("doc id: #{doc.docId}, #cell?[cell id: #it|no cell found for #{doc.geoTag}]")
  }
  println("done")
}
