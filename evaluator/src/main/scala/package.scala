package object evaluator {
  type DocId = String
  type CellId = Int
  case class GeoLoc(lat:Double, lon:Double)
  type Bounds = List[GeoLoc]
  //case class GeoCell(cellId:CellId, bound:Bound)
  case class DocWithLoc(docId:DocId, geoTag:GeoLoc)
  //case class DocWithCell(docId:DocId, cell:GeoCell)
  

  /**a square cell. it has bounds. it determines if a doc is in those bounds.
  * Note: I have no idea if the math is correct.
  */
  case class SquareCell(minLat:Double, maxLat:Double, minLon:Double, maxLon:Double, center:Option[GeoLoc]) {
    def contains(loc:GeoLoc):Boolean = {
      minLat <= loc.lat && loc.lat < maxLat && minLon <= loc.lon && loc.lon < maxLon
    }
  }

  val K_FOR_PR = 50


  /**allows constructing a square cell from a geocell.*/
  case object SquareCell {
    def parse(s:String) = {
    }
  }

  case class LabelWeight(label:String, weight:Double)

  case class LabelPropDoc(id:String, labels:List[LabelWeight])
  type GoldLabel = (String, String)

  case class EvalResults(numDocs:Int, notFound:Int, meanErrDistance:Double, precRecAt:Array[(Int, Double, Double)])
}
