package object getcells {
  type DocId = Int
  type CellId = Int
  case class GeoLoc(lat:Double, lon:Double)
  type Bounds = List[GeoLoc]
  //case class GeoCell(cellId:CellId, bound:Bound)
  case class DocWithLoc(docId:DocId, geoTag:GeoLoc)
  //case class DocWithCell(docId:DocId, cell:GeoCell)
}
