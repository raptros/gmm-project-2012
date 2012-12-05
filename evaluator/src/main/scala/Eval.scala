package evaluator

import scala.collection.immutable.{Stream, PagedSeq, StreamIterator}

//had to put this in this source directory.
import opennlp.textgrounder.util.distances._

@EnhanceStrings
class Evals(goldLabels:String, cellDefs:String) {
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
    val distFromTopToTrue:Double = oDistFromTopToTrue getOrElse (1000000.)
  }

  def evalStream(labelProps:String, seedsFile:String) = {
    val seeds = Loaders.loadSeeds(seedsFile).toSet
    Loaders.loadLabelled(labelProps) flatMap { lpd => if(seeds contains(lpd.id)) None else Some(new EvalItem(lpd))}
  }

  import scala.math.BigInt
  class Aggregator {
    var docCount:Int = 0 
    var distance:Double = 0.0
    val kTruePos:Array[Int] = Array.fill(K_FOR_PR)(0)
    def update(item:EvalItem) = {
      docCount += 1
      distance += item.distFromTopToTrue
      (0 until K_FOR_PR) foreach { i =>
        kTruePos.update(i, kTruePos(i) + (if (0 <= item.trueLoc && item.trueLoc < (i+1)) 1 else 0))
      }
    }
    def gen = Aggregate(docCount, distance, kTruePos)
  }
  case class Aggregate(docCount:Int, distance:Double, kTruePos:Array[Int])


  /** runs aggregator over the item stream, counting up various values.*/
  def aggregate(items:Iterator[EvalItem]):Aggregate = {
    val aggor = new Aggregator
    items foreach (aggor update _)
    aggor gen
  }

  /** carry out the evaluation:
    * load up the stream of eval items
    * aggregate them
    * compute results based on aggregated counts.
    */

  def evaluate(outputs:String, seeds:String):EvalResults = {
    val agg = aggregate(evalStream(outputs,seeds))
    val precRecAt = agg.kTruePos.zipWithIndex map { 
      case (tp, i) => (i, tp.toDouble/(agg.docCount * (i+1)), tp.toDouble/agg.docCount)
    }
    EvalResults(agg.docCount, agg.distance / agg.docCount, precRecAt)
  }
}
