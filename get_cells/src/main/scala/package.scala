package object getcells {
  type DocId = String
  type CellId = Int
  case class GeoLoc(lat:Double, lon:Double)
  type Bounds = List[GeoLoc]
  //case class GeoCell(cellId:CellId, bound:Bound)
  case class DocWithLoc(docId:DocId, geoTag:GeoLoc)
  //case class DocWithCell(docId:DocId, cell:GeoCell)
  
  sealed abstract class LogInfo

  /**a square cell. it has bounds. it determines if a doc is in those bounds.
  * Note: I have no idea if the math is correct.
  */
  case class SquareCell(minLat:Double, maxLat:Double, minLon:Double, maxLon:Double, center:Option[GeoLoc])
  extends LogInfo {
    def contains(loc:GeoLoc):Boolean = {
      minLat <= loc.lat && loc.lat < maxLat && minLon <= loc.lon && loc.lon < maxLon
    }
  }


  /**allows constructing a square cell from a geocell.*/
  case object SquareCell {
    def apply(bounds:Bounds, center:Option[GeoLoc]):SquareCell = {
      val (lats, lons) = {
        val (latBound, lonBound) = bounds.map {p => (p.lat, p.lon)}.unzip
        (latBound.toSet.toList.sorted, lonBound.toSet.toList.sorted)
      }
      SquareCell(lats.head, lats.last, lons.head, lons.last, center)
    }
  }

  case class GeoCell(bounds:Bounds, center:Option[GeoLoc]) extends LogInfo
  case class CellCount(count:Int) extends LogInfo
  case object Junk extends LogInfo

}
