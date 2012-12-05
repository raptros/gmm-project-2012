package evaluator

import scala.collection.immutable.{Stream, PagedSeq}

//had to put this in this source directory.
import opennlp.textgrounder.util.distances._

class Evals(labelProps:String, goldLabels:String, cellDefs:String) {
  lazy val cellMap = Loaders.loadCells(cellDefs).toMap
  lazy val goldMap = Loaders.loadGold(goldLabels).toMap

  implicit def geoLoc2SphereCoord(gl:GeoLoc):SphereCoord = SphereCoord(gl.lat, gl.lon)

  class EvalItem(id:String, labels:List[LabelWeight]) {
    def this(doc:LabelPropDoc) = this(doc.id, doc.labels)

    val oTrueLoc = for { 
      gold <- (goldMap get id)
    } yield labels indexWhere(_.label == gold)

    val oDistFromTopToTrue = for {
      gold <- (goldMap get id)
      trueCell <- (cellMap get gold)
      trueCenter <- trueCell.center
      top <- labels.headOption
      topCell <- (cellMap get top.label)
      topCenter <- topCell.center
    } yield spheredist(trueCenter, topCenter)

    /** Which item is the true cell according to the gold labelling. -1 if not present. */
    val trueLoc:Int = oTrueLoc getOrElse(-1)
    /** Distance in km (accounting for globe) between true and first predicted points */
    val distFromTopToTrue:Double = oDistFromTopToTrue getOrElse(1000000.)
  }


  def evalStream = Loaders.loadLabelled(labelProps) map (new EvalItem(_))



}
