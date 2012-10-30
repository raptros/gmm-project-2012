package getcells

import scala.collection.immutable.{Stream, PagedSeq}
import scala.util.parsing.combinator._
import scala.util.parsing.input.PagedSeqReader

import java.io.{Reader, File, FileReader}

sealed abstract class LogInfo

case class GeoCell(bounds:Bounds, center:GeoLoc) extends LogInfo
case class CellCount(count:Int) extends LogInfo
case object Junk extends LogInfo


object LogParser extends StreamParser[LogInfo] {
  def eol = '\n'
  def junk = """.*""".r

  def datathing = """\w+(\.\w+)?\s+\w+(\(\w+\))?""".r

  def geocell = """^#\d+:\s+[^:]*:""".r ~> "GeoCell" ~> "(" ~> coordsList ~ center <~ "," <~ repsep(datathing, ",") <~ ")" ^^ {
    case cl ~ cntr => GeoCell(cl, cntr)
  }
  def coordsList = "List" ~> "(" ~> rep1sep(coord, "," ) <~ ")"
  def coord = "(" ~> floatingPointNumber ~ ("," ~> floatingPointNumber) <~ ")" ^^ {
    case lat ~ lon => GeoLoc(lat.toDouble, lon.toDouble)
  }
  def center = "(" ~> "Center" ~> ":" ~> coord <~ ")"

  def countLine:Parser[CellCount] = "Total number of cells:" ~> wholeNumber <~ eol ^^ (cnt => CellCount(cnt.toInt))
  def cellLine:Parser[GeoCell] = geocell <~ eol
  def junkLine:Parser[LogInfo] = junk ~> eol ^^^ (Junk)

  def line = countLine | cellLine | junkLine
  
  def streamItem = line

  val grouper = (info:LogInfo) => info match {
    case (cc:CellCount) => 0
    case (gc:GeoCell) => 1
    case _ => 2
  }
  def loadLogCells(logpath:String) = {
    val infile = new File(logpath)
    val reader = new FileReader(infile)
    val stream = extract(reader)
    val grouped = stream.filterNot(_==Junk).groupBy(grouper)
    val count = (grouped get 0).flatten.head.asInstanceOf[CellCount].count
    val items = grouped(1).toSet
    if (count == items.size) {} else println("missing " + (count - items.size) + " cells")
    items
  }
}
