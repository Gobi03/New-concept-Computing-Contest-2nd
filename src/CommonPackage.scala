object CommonPackage {

  case class Answer(V: Int, Vemb: Int, sideEmb: Int) {
    // if empty, value become zero.
    // value means an element of original graph
    val elm = Array.ofDim[Int](Vemb+1, Vemb+1)

    def toArray: Array[Long] = {
      val res = new Array[Long](V+1)
      for (y <- 1 to sideEmb; x <- 1 to sideEmb) {
        if (elm(y)(x) > 0) {
          val kingsNode: Long = (y-1)*sideEmb.toLong + x
          res(elm(y)(x)) = kingsNode
        }
      }

      res
    }


    def fromArray(ar: Array[Long]): Answer = {
      require(ar.length == V+1)

      for (i <- 1 to V) {
        val now: Long = ar(i)
        val y = if (now % sideEmb == 0) now/sideEmb else now/sideEmb + 1
        val x = if (now % sideEmb == 0) sideEmb else now % sideEmb
        elm(y.toInt)(x.toInt) = i
      }

      this
    }
  }
}

