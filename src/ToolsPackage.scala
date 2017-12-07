object ToolsPackage {
  import CommonPackage._
  import ResultPackage._

  trait ToolsTrait {
    val side: Int
    val sideEmb: Int

    

    def inField(pos: Point): Boolean
  }

  class Tools(infos: InputInfos) extends ToolsTrait {
    import Math._
    import infos._

    val side = sqrt(V).ceil.toInt  // 正方形に置く場合の一辺の長さ
    val sideEmb = sqrt(Vemb).round.toInt

    def inField(pos: Point): Boolean = {
      pos.x >= 1 && pos.y >= 1 &&
      pos.x <= sideEmb && pos.y <= sideEmb
    }

    implicit class RichPoint(pos: Point) {
      def toNodeEmb: Int = (pos.y-1)*sideEmb + pos.x
    }

  }
}
