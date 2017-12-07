object ResultPackage {
  import CommonPackage._

  type Node = Int  // original-graph's node-number
  type KingsNode = Int  // kings-graph's node-number

  trait ResultTrait {

    val elm: Array[Array[Node]]         // store node
    def toListArray: Array[List[KingsNode]]  // store kingsNodes

    // 
    def get(pos: Point): Int
    // get a node by Kings-Node number
    def getByKingsNode(nodeEmb: Int): Int

    def calcScore(): Long

  }

  class Result(initKings: Array[Array[Node]], side: Int, sideEmb: Int)
              (implicit infos: InputInfos) extends ResultTrait {
    import infos._

    val elm = initKings
    require(initKings.length == sideEmb+1 && initKings(0).length == sideEmb+1)

    def get(pos: Point): Node = elm(pos.y)(pos.x)

    def getByKingsNode(nodeEmb: Int): Node = {
      val pos: Point = this.kingsnodeToPos(nodeEmb)
      this.get(pos)
    }

    def toListArray: Array[List[KingsNode]] = {
      val res = Array.fill[List[Int]](V+1)(Nil)
      for (y <- 1 to sideEmb; x <- 1 to sideEmb) {
        if (elm(y)(x) > 0) {
          val kingsNode = (y-1)*sideEmb + x
          res(elm(y)(x)) = kingsNode :: res(elm(y)(x))
        }
      }
      res
    }

    def calcScore(): Long = {

      val Bonus = 100000L
      val init = 5000L     

      val edgePoint: Long = {
        var res: Set[(Node, Node)] = Set()
        for {
          nodeEmb: KingsNode <- 1 to Vemb
          node = this.getByKingsNode(nodeEmb)
          if node > 0
        } {
          val actualEdges: Set[(Node, Node)] =
            infos.edges(node).map(makeAscPair(_, node)).toSet

          val checkEdges: Set[(Node, Node)] =
            edgesEmb(nodeEmb)
              .filter(_ > nodeEmb)  // looks only larger kings-nodes
              .map(this.getByKingsNode(_))
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
        this.toListArray.slice(1, V+1).toList.map {
          case Nil => throw new Exception("non-linked node exists")
          case links => links.length - 1L
        }
        .sum
      }

      // TODO: check 0-point condition

      init + edgePoint - loss
    }

    private def kingsnodeToPos(nodeEmb: KingsNode): Point = {
      val y = if (nodeEmb % sideEmb == 0) nodeEmb/sideEmb
              else nodeEmb/sideEmb + 1
      val x = if (nodeEmb % sideEmb == 0) sideEmb
              else nodeEmb % sideEmb
      Point(x, y)
    }

    private def makeAscPair(a: Node, b: Node): (Node, Node) = {
      require(a != b)
      if (a < b)
        (a, b)
      else
        (b, a)
    }

  }

}
