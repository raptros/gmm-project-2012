package getcells

import scala.collection.immutable.{Stream, PagedSeq}
import scala.util.parsing.combinator._
import scala.util.parsing.input.PagedSeqReader
import scala.util.matching.Regex

import java.io.{Reader, File, FileReader}

import java.io.{Reader, File, FileReader, BufferedReader}

object StringStream {
  @inline protected def die(reader:Reader, msg:Option[String]=None) = {msg foreach (println _); reader.close; Stream.empty}
  def apply(reader:Reader):Stream[String] = {
    readNext(new BufferedReader(reader))
  }
  def readNext(br:BufferedReader):Stream[String]= try {
    Option(br.readLine()) match {
      case Some(line) => line #:: readNext(br)
      case None => die(br)
    }
  }
  catch {
    case _ => die(br, Some("exception"))
  }
}

/** line based file parser. */
abstract class LineStreamParser[A] extends JavaTokenParsers {
  /**neat little way to close a reader and provide end of stream, with optional message.*/
  @inline protected def die(reader:Reader, msg:Option[String]=None) = {msg foreach (println _); reader.close; Stream.empty}

  /** Produces a stream over items, using parser combinators*/
   def extract(in:Reader):Stream[A] = StringStream(in) flatMap { line =>
     parseAll(streamItem, line) match {
       case Success(result, _) => Some(result)
       case NoSuccess(msg, rest) => None
     }
   }

  def streamItem:Parser[A]
}

abstract class StreamParser[A] extends JavaTokenParsers {
  /**neat little way to close a reader and provide end of stream, with optional message.*/
  @inline protected def die(reader:Reader, msg:Option[String]=None) = {msg foreach (println _); reader.close; Stream.empty}

  /** Produces a stream over items, using parser combinators*/
  def extract(in:Reader):Stream[A] = {
     val pagedSeq = PagedSeq.fromReader(in)
     val pagedSeqReader = new PagedSeqReader(pagedSeq)
     extractNext(pagedSeqReader, in)
  }

  /** recursive Stream builder */
  def extractNext(input:Input, reader:Reader):Stream[A] = if (input atEnd) die(reader)
  else parse(streamItem, input) match {
    case Success(result, rest) => result #:: extractNext(rest, reader)
    case NoSuccess(msg, rest) => die(reader, Some(msg)) //give up
  }

  def streamItem:Parser[A]
}

/**parses out info from the coords document in wikipedia data*/
object CoordsParser extends StreamParser[Option[DocWithLoc]] {
  def clean(s:String):String = {
    val s1 = s.toLowerCase.trim
    val s2 = alphanum.replaceAllIn(s1, "")
    spaces.replaceAllIn(s2, " ")
  }

  val alphanum = """[^\w0-9 ]+""".r ///blank
  val spaces = """\s+|\-+""".r // single space


  def front = "Article"
  def title = front ~> "title:" ~> """.*""".r <~ '\n' ^^ {
    t => clean(t)
  }

  def id = front ~> "ID:" ~> wholeNumber <~ '\n' ^^ { _.toInt }
  def coordinates = front ~> "coordinates:" ~> (coordPair | noCoord)<~'\n'

  def coordPair = floatingPointNumber ~ ("," ~> floatingPointNumber) ^^ {
    case lat ~ lon => Some(GeoLoc(lat.toDouble, lon.toDouble))
  }
  def noCoord = """.*""".r ^^^ { None }

  def article = title ~ id ~ coordinates ^^ {
    case t ~ i ~ None => None
    case t ~ i ~ Some(c) => Some(DocWithLoc(t, c))
  }
  def streamItem = article

  def loadDocs(filename:String):Stream[DocWithLoc] = {
    val infile = new File(filename)
    val reader = new FileReader(infile)
    extract(reader).flatten
  }
}

/**extracts cells from textgrounder logs*/
object LogParser extends LineStreamParser[LogInfo] {
  def eol = """$""".r
  def junk = """.*""".r

  def datathing = """\w+(\.\w+)?\s+\w+(\(\w+\))?""".r

  def cellThingName = "GCell"

  def cellstart1 = """^#\d+:\s+[^:]*:""".r 
  def cellstart2 = """^.*cell""".r
  def cellstart = cellstart1 | cellstart2

  def geocell = cellstart ~> cellThingName ~> "(" ~> coordsPair ~ ("," ~> coord) <~ "," <~ repsep(datathing, ",") <~ ")" ^^ {
    case sq ~ gl => sq.copy(center = Some(gl))
  }
 def coordsPair:Parser[SquareCell] = coord ~ ("-" ~> coord) ^^ {
    case GeoLoc(lat1, lon1) ~ GeoLoc(lat2, lon2) => SquareCell(lat1, lat2, lon1, lon2, None)
  }
  def coordsList = rep1sep(coord, "-" ) ^^ {
    coords => coords.toSet.toList //remove duplicate coordinate points.
  }
  def coord:Parser[GeoLoc] = "(" ~> floatingPointNumber ~ ("," ~> floatingPointNumber) <~ ")" ^^ {
    case lat ~ lon => GeoLoc(lat.toDouble, lon.toDouble)
  }

  def countLine:Parser[CellCount] = "Total number of cells:" ~> wholeNumber <~ eol ^^ (cnt => CellCount(cnt.toInt))
  def cellLine:Parser[SquareCell] = geocell <~ eol
  def junkLine:Parser[LogInfo] = junk ~> eol ^^^ (Junk)

  def line = countLine | cellLine | junkLine
  
  def streamItem = line

  type Stuff = (Option[Int], Set[SquareCell])
  def group(stuff:Stuff, info:LogInfo):Stuff = info match {
    case CellCount(amt) => (Some(amt), stuff._2)
    case GeoCell(bounds, center)  => (stuff._1, stuff._2 + SquareCell(bounds, center)) 
    case sq:SquareCell  => (stuff._1, stuff._2 + sq) 
    case Junk => stuff
  }

  def loadLogCells(logpath:String):Set[SquareCell] = {
    val infile = new File(logpath)
    val reader = new FileReader(infile)
    val stream = extract(reader)
    println("stream obtained")
    val init = Pair(None:Option[Int], Set.empty[SquareCell])
    val (oCount, cells):Stuff = stream.foldLeft(init)(group(_, _))
    val count = oCount getOrElse 0
    if (count == cells.size) {} else println("missing " + (count - cells.size) + " of " + (count) + " cells")
    cells
  }
}
