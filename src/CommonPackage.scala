object CommonPackage {

  def getNowTime: Long = System.currentTimeMillis

  def makeTornadePoints(side: Int): List[Point] = {
    def func(leftUp: Int, rightDown: Int, res: List[Point]): List[Point] = {
      if (leftUp == rightDown) {
        (Point(leftUp, rightDown) :: res).reverse
      }
      else if (leftUp > rightDown) {
        res.reverse
      }
      else {
        val next: List[Point] = (leftUp to rightDown-1).foldLeft(res){
          (acc, i) => Point(i, leftUp) :: Point(rightDown, i) ::
                      Point(i+1, rightDown) :: Point(leftUp, i+1) :: acc
        }

        func(leftUp+1, rightDown-1, next)
      }
    }

    func(1, side, Nil)
  }

  case class Point(val x: Int, val y: Int) {
    def this(coord: (Int, Int)) = this(coord._1, coord._2)
    def toPair: (Int, Int) = (x, y)

    def +(p: Point): Point =
      new Point(this.x + p.x, this.y + p.y)
    def -(p: Point): Point =
      new Point(this.x - p.x, this.y - p.y)

    def inField(sideEmb: Int): Boolean = {
      this.x >= 1 && this.y >= 1 &&
      this.x <= sideEmb && this.y <= sideEmb
    }

    def move8: List[Point] = {
      var res: List[Point] = Nil
      for (volx <- -1 to 1; voly <- -1 to 1) {
        if ((x, y) != (0, 0))
          res = Point(this.x + volx, this.y + voly) :: res
      }
      res
    }

  }

  case class InputInfos(
    val V: Int, val E: Int,
    val edges: Array[List[Int]],
    val Vemb: Int, val Eemb: Int,
    val edgesEmb: Array[List[Int]]
  )

}

