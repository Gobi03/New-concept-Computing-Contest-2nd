import java.io._
import java.util.StringTokenizer
import CommonPackage._
import ResultPackage._
import ToolsPackage._
import EvaluatorPackage._

object Main extends App {
  val startTime: Long = getNowTime

  import MainFuncs._

  /*** input ***/
  val in = new InputReader(System.in)
  
  val V, E = in.next().toInt
  val edges = Array.fill[List[Int]](V+1)(Nil)
  for (_ <- 1 to E) {
    val u, v = in.next().toInt
    edges(u) = v :: edges(u)
    edges(v) = u :: edges(v)
  }

  val Vemb, Eemb = in.next().toInt
  val edgesEmb = Array.fill[List[Int]](Vemb+1)(Nil)
  for (_ <- 1 to Eemb) {
    val a, b = in.next().toInt
    edgesEmb(a) = b :: edgesEmb(a)
    edgesEmb(b) = a :: edgesEmb(b)
  }


  /*** process ***/
  implicit val inputInfos =
    InputInfos(V, E, edges.clone, Vemb, Eemb, edgesEmb.clone)
  val tools = new Tools(inputInfos)
  val (side, sideEmb) = (tools.side, tools.sideEmb)

  // if empty, value become zero.
  // value means an element of original graph
  val initResult = new Result(mkInitialGraph(side, sideEmb), side, sideEmb)

  val evaluator = new Evaluator(side, sideEmb)
  val result = evaluator.main(initResult, startTime)

  val answer: Answer = Answer(result)


  /*** output ***/
  printer(answer, V)
}


object MainFuncs {
  /* output */
  def printer(answer: Answer, V: Int): Unit = {
    import java.io._

    val pw = new PrintWriter(System.out)
    val answerArray: Array[List[Int]] = answer.toArray

    for (i <- 1 to V) {
      pw.println(s"1 ${answerArray(i).mkString(" ")}")
    }
    pw.flush()
    pw.close()
  }

  // make initialize Kings-Graph
  def mkInitialGraph(side: Int, sideEmb: Int)  
                    (implicit infos: InputInfos): Array[Array[Int]] = {
    // deprecatedes

    import infos._

    val res = Array.ofDim[Int](sideEmb+1, sideEmb+1)

    val tornades: List[Point] = makeTornadePoints(side)
    require(tornades.length == side * side)

    type EdgesNum = Int
    var list: List[(EdgesNum, Node)] = Nil
    for (i <- 1 to V) {
      list = (edges(i).length, i) :: list
    }

    tornades
      .slice(tornades.length - V, tornades.length)
      .zip(list.sorted)
      .foreach {
        case (pos, (_, node)) =>
          res(pos.y)(pos.x) = node
      }

    res
  }

  case class Answer(result: Result)(implicit infos: InputInfos) {
    import infos._

    def toArray: Array[List[Int]] = result.toListArray
  }
}

class InputReader(stream: InputStream) {
  private val reader = new BufferedReader(new InputStreamReader(stream))
  private var tokenizer: StringTokenizer = new StringTokenizer(reader.readLine())
  def next(): String = {
    while (!tokenizer.hasMoreTokens()) {
      tokenizer = new StringTokenizer(reader.readLine())
    }
    tokenizer.nextToken()
  }
}
