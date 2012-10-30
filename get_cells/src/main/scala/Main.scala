package getcells
import scala.collection.immutable.{Stream, PagedSeq}
import scala.util.parsing.combinator._
import scala.util.parsing.input.PagedSeqReader

import java.io.{Reader, File, FileReader}

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


object CoordsParser extends StreamParser[DocWithLoc] {
  def front = "Article"
  def title = front ~> "title:" ~> """.*""".r <~ '\n'
  def id = front ~> "ID:" ~> wholeNumber <~ '\n' ^^ { _.toInt }
  def coordinates = front ~> "coordinates:" ~> floatingPointNumber ~ ("," ~> floatingPointNumber) <~ '\n' ^^ {
    case lat ~ lon => GeoLoc(lat.toDouble, lon.toDouble)
  }
  def article = title ~ id ~ coordinates ^^ {
    case t ~ i ~ c => DocWithLoc(i, c)
  }
  def streamItem = article
}

object GetCells extends App {
  val cells = LogParser.loadLogCells(args(0))
  cells foreach(println)
}
