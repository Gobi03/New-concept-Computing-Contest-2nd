object ToolsPackage {
  import CommonPackage._
  import ResultPackage._

  trait ToolsTrait {
    val side: Int
    val sideEmb: Int
  }

  class Tools(infos: InputInfos) extends ToolsTrait {
    import Math._
    import infos._

    val side = sqrt(V).ceil.toInt  // 正方形に置く場合の一辺の長さ
    val sideEmb = sqrt(Vemb).round.toInt

    implicit class RichPoint(pos: Point) {
      def toNodeEmb: Int = (pos.y-1)*sideEmb + pos.x
    }

  }

}
