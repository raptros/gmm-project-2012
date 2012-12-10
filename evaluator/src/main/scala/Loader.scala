package evaluator

import scala.collection.immutable.{Stream, PagedSeq}
import scala.util.parsing.combinator._
import scala.util.parsing.input.PagedSeqReader
import scala.util.matching.Regex
import scala.io.Source

import java.io.{Reader, File, FileReader}
import java.io.{Reader, File, FileReader, BufferedReader}

/** parses out square cells and other case classes*/
object CellParser extends JavaTokenParsers {
  def double:Parser[Double] = floatingPointNumber ^^ (d => d.toDouble)
  def geoLoc:Parser[GeoLoc] = "GeoLoc(" ~> double ~ ("," ~> double) <~ ")" ^^ {
    case lat ~ lon => GeoLoc(lat, lon)
  }
  def optGeoLoc:Parser[Option[GeoLoc]] = ("None" ^^ {_ => None}) | ("Some(" ~> geoLoc <~ ")" ^^ {l => Some(l)})
  def squareCell:Parser[SquareCell] = "SquareCell(" ~> double ~ 
  ("," ~> double) ~ ("," ~> double) ~ ("," ~> double) ~ ("," ~> optGeoLoc) <~ ")" ^^ {
    case lat1 ~ lat2 ~ lon1 ~ lon2 ~ oCen => SquareCell(lat1, lat2, lon1, lon2, oCen)
  }

  def parseSquareCell(sqs:String):SquareCell = parseAll(squareCell, sqs).get
}

/** line based file parser. */
abstract class LineParser[A] extends JavaTokenParsers {
  /**neat little way to close a reader and provide end of stream, with optional message.*/
  @inline protected def die(reader:Reader, msg:Option[String]=None) = {msg foreach (println _); reader.close; Stream.empty}

  /** Produces a stream over items, using parser combinators*/
  def extract(in:String):Iterator[A] = Source.fromFile(in).getLines.flatMap { line =>
    parseAll(lItem, line) match {
      case Success(result, _) => Some(result)
      case NoSuccess(msg, rest) => None
    }
  }

  def lItem:Parser[A]
}

object QuotedsFile extends LineParser[List[String]] {
  def innerStringLiteral: Parser[String] = "\"" ~> """([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*""".r <~ "\""
  def lItem = rep1sep(innerStringLiteral, regex(",".r))
}

/** parses output of label propagation
  * cf https://github.com/parthatalukdar/junto/blob/master/examples/simple/simple_config 
  */
object LabelLineParsers {
  def extract(fname:String):Iterator[LabelPropDoc] = Source.fromFile(fname).getLines map { line =>
    //val line2 = """[\s]+""".r replaceAllIn(line, " ")
    val ll = line.split(Array(' ', '\t')).toList.filterNot(_.isEmpty)
    LabelPropDoc(ll.head,
      ll.tail.grouped(2).flatMap {
        case List(l, w) => Some(LabelWeight(l, w.toDouble))
        case _ => None
      }.toList.dropRight(1).takeRight(100).filterNot(_.label == "__DUMMY__"))
  }
}

object GoldLabelLineParsers extends LineParser[GoldLabel] {
  def lItem = """\w+""".r ~ """[\w\d_]+""".r  <~ floatingPointNumber ^^ { case i ~ l => i -> l}
}

object Seeds {
  def extract(fname:String):Iterator[String] = Source.fromFile(fname).getLines map { line =>
    val ll = line.split(Array(' ', '\t')).toList.filterNot(_.isEmpty)
    ll(0)
  }
}

object Loaders {
  def open(path:String):Reader = new FileReader(new File(path))
  /** loads up a stream of cell id, square cells from a file generated by get_cells.*/
  def loadCells(cellsFile:String):Iterator[(String, SquareCell)] = {
    QuotedsFile.extract(cellsFile) map {
      case List(s1, s2) => (s1 -> CellParser.parseSquareCell(s2))
      case _ => throw new Exception("loadCells broke because QuotedsFile produced something weird")
    }
  }

  /** given a path to the locs file produced by get_cells, returns a stream over (article id/title, cell id) pairs*/
  def loadDocLocs(locsFile:String):Iterator[(String, String)] = {
    QuotedsFile.extract(locsFile) map {
      case List(s1, s2) => (s1 -> s2)
      case _ => throw new Exception("loadCells broke because QuotedsFile produced something weird")
    }
  }

  def loadLabelled(lpFile:String):Iterator[LabelPropDoc] = LabelLineParsers.extract(lpFile)

  def loadGold(goldFile:String):Iterator[GoldLabel] = GoldLabelLineParsers.extract(goldFile)
  def loadSeeds(seedsFile:String):Iterator[String] = Seeds.extract(seedsFile)
}
