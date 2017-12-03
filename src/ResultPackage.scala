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
  }

  class Result(side: Int, sideEmb: Int)(implicit infos: InputInfos) extends ResultTrait {
    import infos._

    val elm = Array.ofDim[Node](sideEmb+1, sideEmb+1)
    initialize()

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

    // initialize elm
    private def initialize(): Unit = {
      // deprecatedes

      val answerArray = Array.fill[List[Int]](V+1)(Nil)
      // make square-like answer
      for (i <- 1 to V) {
        val vol = if (i % side == 0) side else i % side
        answerArray(i) = ((i-1) / side) * sideEmb + vol :: answerArray(i)
      }
      // apply answerArray to elm
      for (i <- 1 to V; now <- answerArray(i)) {
        val y = if (now % sideEmb == 0) now/sideEmb else now/sideEmb + 1
        val x = if (now % sideEmb == 0) sideEmb else now % sideEmb
        elm(y.toInt)(x.toInt) = i
      }
    }

    private def kingsnodeToPos(nodeEmb: KingsNode): Point = {
      val y = if (nodeEmb % sideEmb == 0) nodeEmb/sideEmb
              else nodeEmb/sideEmb + 1
      val x = if (nodeEmb % sideEmb == 0) sideEmb
              else nodeEmb % sideEmb
      Point(x, y)
    }

  }

}
