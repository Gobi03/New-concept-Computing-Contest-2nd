object ToolsPackage {
  import CommonPackage._
  import ResultPackage._

  class Tools(infos: InputInfos) {
    import Math._
    import infos._

    val side = sqrt(V).ceil.toInt  // 正方形に置く場合の一辺の長さ
    val sideEmb = sqrt(Vemb).round.toInt

    def getNowTime: Long = System.currentTimeMillis

    def calcScore(result: Result): Long = {

      val Bonus = 100000L
      val init = 5000L     

      val edgePoint: Long = {
        var res: Set[(Node, Node)] = Set()
        for {
          nodeEmb: KingsNode <- 1 to Vemb
          node = result.getByKingsNode(nodeEmb)
          if node > 0
        } {
          val actualEdges: Set[(Node, Node)] =
            infos.edges(node).map(makeAscPair(_, node)).toSet

          val checkEdges: Set[(Node, Node)] =
            edgesEmb(nodeEmb)
              .filter(_ > nodeEmb)  // looks only larger kings-nodes
              .map(result.getByKingsNode(_))
              .filter(nextNode => nextNode > 0 && nextNode != node) // validation
              .map(makeAscPair(_, node))
              .toSet

          val nowEdges = actualEdges intersect checkEdges
          res = res union nowEdges
        }

        val base = res.size.toLong * 100L
        if (res.size == infos.E)
          base + Bonus
        else
          base
      }

      val loss: Long = {
        result.toListArray.slice(1, V+1).toList.map {
          case Nil => throw new Exception("non-linked node exists")
          case links => links.length - 1L
        }
        .sum
      }

      // TODO: check 0-point condition

      init + edgePoint - loss
    }

    def inField(pos: Point): Boolean = {
      pos.x >= 1 && pos.y >= 1 &&
      pos.x <= sideEmb && pos.y <= sideEmb
    }

    private def makeAscPair(a: Node, b: Node): (Node, Node) = {
      require(a != b)
      if (a < b)
        (a, b)
      else
        (b, a)
    }

    implicit class RichPoint(pos: Point) {
      def toNodeEmb: Int = (pos.y-1)*sideEmb + pos.x
    }

  }
}
