object CommonPackage {

  case class Answer(V: Int, Vemb: Int, sideEmb: Int) {
    // if empty, value become zero.
    // value means an element of original graph
    val elm = Array.ofDim[Int](sideEmb+1, sideEmb+1)

    def toArray: Array[List[Int]] = {
      val res = Array.fill[List[Int]](V+1)(Nil)
      for (y <- 1 to sideEmb; x <- 1 to sideEmb) {
        if (elm(y)(x) > 0) {
          val kingsNode = (y-1)*sideEmb + x
          res(elm(y)(x)) = kingsNode :: res(elm(y)(x))
        }
      }

      res
    }


    def fromArray(ar: Array[List[Int]]): Answer = {
      require(ar.length == V+1)

      // initialize
      for (y <- 1 to sideEmb; x <- 1 to sideEmb)
        elm(y)(x) = 0

      for (i <- 1 to V) {
        for (nodeEmb <- ar(i)) {
          val y = if (nodeEmb % sideEmb == 0) nodeEmb/sideEmb else nodeEmb/sideEmb + 1
          val x = if (nodeEmb % sideEmb == 0) sideEmb else nodeEmb % sideEmb
          elm(y)(x) = i
        }
      }

      this
    }
  }
}

