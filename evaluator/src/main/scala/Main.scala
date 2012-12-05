
package evaluator

trait TestPaths {
  def labelProps:String
  def goldLabels:String
  def cellDefs:String
  def newEvals:Evals = new Evals(goldLabels, cellDefs)
}

object TestPathsAidan extends TestPaths {
  val dataDir = "/home/aidan/Projects/data/gmm"
  val labelProps = dataDir + "/sample_500_label_prop_output"
  val goldLabels = dataDir + "/labels_out"
  val cellDefs = dataDir + "/cells.txt"
  val outputs = List(
    "label_prop_output_12.5",
    "label_prop_output_25",
    "label_prop_output_50"
    //"label_prop_output_75"
  ) map (dataDir + "/" +_)
  val seeds = List(
    "seeds12.5",
    "seeds25",
    "seeds50"
    //"seeds75"
  ) map (dataDir + "/" +_)
  val evals = outputs zip seeds
  //val evals = List(dataDir + "/sample_500_label_prop_output")
}


@EnhanceStrings
object TestEval extends App {
  val evals = TestPathsAidan.newEvals
  val results = TestPathsAidan.evals map {
    case (o, s) => evals.evaluate(o, s)
  }
  results foreach { res =>
    println("doc count: #res.numDocs")
    println("mean error dist: #res.meanErrDistance")
    println("k\tprec\trec:")
    res.precRecAt foreach {
      case (i, p, r) => println("#{{i+1}}\t#p\t#r")
    }
  }
}
