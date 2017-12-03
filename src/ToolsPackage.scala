object ToolsPackage {
  import CommonPackage._

  class Tools(infos: InputInfos) {
    import Math._
    import infos._

    val side = sqrt(V).ceil.toInt  // 正方形に置く場合の一辺の長さ
    val sideEmb = sqrt(Vemb).round.toInt

    def getNowTime: Long = System.currentTimeMillis

    def checkScore(kingsGraph: Array[Array[Int]]) = ???
  }

}
