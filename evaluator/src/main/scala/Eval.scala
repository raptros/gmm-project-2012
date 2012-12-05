package evaluator

import scala.collection.immutable.{Stream, PagedSeq}

//had to put this in this source directory.
import opennlp.textgrounder.util.distances._

@EnhanceStrings
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

  def evalStream() = Loaders.loadLabelled(labelProps) map (new EvalItem(_))

  import scala.math.BigInt
  case class Aggregator(docCount:Int, distance:Double, kTruePos:Array[Int])

  case object Aggregator {
    def start:Aggregator = new Aggregator(0, 0.0, Array.fill(K_FOR_PR)(0))
  }

  /** folds Aggregator over the item stream, counting up various values.*/
  def aggregate(items:Stream[EvalItem]):Aggregator = items.foldLeft(Aggregator.start) {
    (agg:Aggregator, item:EvalItem) => Aggregator(agg.docCount + 1, agg.distance + item.distFromTopToTrue,
      agg.kTruePos.zipWithIndex map {
        case (v, i) => if (0 <= item.trueLoc && item.trueLoc < (i+1)) v+1 else v
      })
  }

  /** carry out the evaluation:
    * load up the stream of eval items
    * aggregate them
    * compute results based on aggregated counts.
    */
  def evaluate():EvalResults = {
    val stream = evalStream()
    val agg = aggregate(stream)
    val precRecAt = agg.kTruePos.zipWithIndex map { 
      case (tp, i) => (i, tp.toDouble/(agg.docCount * (i+1)), tp.toDouble/agg.docCount)
    }
    EvalResults(agg.docCount, agg.distance / agg.docCount, precRecAt)
  }
}
