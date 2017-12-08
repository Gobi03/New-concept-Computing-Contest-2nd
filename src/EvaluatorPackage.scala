object EvaluatorPackage {
  import CommonPackage._
  import ResultPackage._
  import scala.util.Random
  import scala.annotation.tailrec

  trait EvaluatorTrait {
    type Node = Int
    type KingsGraph = Array[Array[Int]]

    val endTime = 29L * 1000
    val rand = new Random()

    // get unconnected-state-nodes
    def getStandAloneNodes(result: Result): List[Int]

    def getConnectedNodes(result: Result, pos: Point): List[Int]
    def getConnectedNodes(result: Result, node: Node): List[Int]
  }

  class Evaluator(side: Int, sideEmb: Int)  
                 (implicit infos: InputInfos) extends EvaluatorTrait {
    import infos._

    @tailrec
    final def main(result: Result, startTime: Long): Result = {
      val next = result.copy
      val targets: List[Node] = this.getStandAloneNodes(next)

      if (targets.length >= 2) {
        targets.zip(rand.shuffle(targets)).foreach {
          case (node1, node2) => next.swap(node1, node2)
        }
      }
      else
        return result

      val adoptResult =
        if (result.calcScore >= next.calcScore)
          result
        else
          next

      if (getNowTime - startTime > endTime)
        adoptResult
      else
        main(adoptResult, startTime)
    }

    
    def getStandAloneNodes(result: Result): List[Int] = {
      for {
        y <- (1 to sideEmb).toList
        x <- (1 to sideEmb).toList
        here = Point(x, y)
        node = result.get(here)
        if node > 0
        aroundNodes: Set[Node] = result.getAroundNodes(here).toSet
        connectableNodes: Set[Node] = edges(node).toSet
        if (aroundNodes intersect connectableNodes).size == 0
      } yield node
    }

    // get connected-nodes by Position
    def getConnectedNodes(result: Result, pos: Point): List[Int] = {
      require(pos.inField(sideEmb) && result.get(pos) > 0)

      val node = result.get(pos)
      edges(node)
    }
    def getConnectedNodes(result: Result, node: Node): List[Int] = {
      require(node <= V)
      edges(node)
    }

  }
}
