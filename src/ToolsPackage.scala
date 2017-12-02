object ToolsPackage {

  class Tools(
    val V: Int, val E: Int,
    val edges: Array[List[Int]],
    val Vemb: Int, val Eemb: Int,
    val edgesEmb: Array[List[Int]]
  ) {
    import Math._

    val side = sqrt(V).ceil.toInt  // 正方形に置く場合の一辺の長さ
    val sideEmb = sqrt(Vemb).round.toInt

    def getNowTime: Long = System.currentTimeMillis

    def checkScore(kingsGraph: Array[Array[Int]]) = ???
  }

}
