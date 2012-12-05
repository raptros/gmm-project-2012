
package evaluator

trait TestPaths {
  def labelProps:String
  def goldLabels:String
  def cellDefs:String
  def newEvals:Evals = new Evals(labelProps, goldLabels, cellDefs)
}

object TestPathsAidan extends TestPaths {
  val dataDir = "/home/aidan/Projects/data/gmm"
  val labelProps = dataDir + "/sample_500_label_prop_output"
  val goldLabels = dataDir + "/labels_out"
  val cellDefs = dataDir + "/cells.txt"
}

@EnhanceStrings
object TestEval extends App {
  val evals = TestPathsAidan.newEvals
  val res = evals.evaluate()
  println("doc count: #res.numDocs")
  println("mean error dist: #res.meanErrDistance")
  println("k\tprec\trec:")
  res.precRecAt foreach {
    case (i, p, r) => println("#{{i+1}}\t#p\t#r")
  }
}
