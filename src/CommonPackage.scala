object CommonPackage {

  case class Point(val x: Int, val y: Int) {
    def this(coord: (Int, Int)) = this(coord._1, coord._2)
    def toPair: (Int, Int) = (x, y)

    def +(p: Point): Point =
      new Point(this.x + p.x, this.y + p.y)
    def -(p: Point): Point =
      new Point(this.x - p.x, this.y - p.y)
  }

  case class InputInfos(
    val V: Int, val E: Int,
    val edges: Array[List[Int]],
    val Vemb: Int, val Eemb: Int,
    val edgesEmb: Array[List[Int]]
  )

  case class Answer(result: Array[Array[Int]], sideEmb: Int)(implicit infos: InputInfos) {
    import infos._

    def toArray: Array[List[Int]] = {
      val res = Array.fill[List[Int]](V+1)(Nil)
      for (y <- 1 to sideEmb; x <- 1 to sideEmb) {
        if (result(y)(x) > 0) {
          val kingsNode = (y-1)*sideEmb + x
          res(result(y)(x)) = kingsNode :: res(result(y)(x))
        }
      }
      res
    }


    // deprecated
    def fromArray(ar: Array[List[Int]]): Answer = {
      require(ar.length == V+1)

      val res = Array.ofDim[Int](sideEmb+1, sideEmb+1)
      for (i <- 1 to V) {
        for (nodeEmb <- ar(i)) {
          val y = if (nodeEmb % sideEmb == 0) nodeEmb/sideEmb
                  else nodeEmb/sideEmb + 1
          val x = if (nodeEmb % sideEmb == 0) sideEmb
                  else nodeEmb % sideEmb
          res(y)(x) = i
        }
      }

      Answer(res, sideEmb)
    }
  }
}

