import java.io._
import java.util.StringTokenizer
import CommonPackage._

object Main extends App {
  import MainFuncs._

  val in = new InputReader(System.in)

  /* input */
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


  /* process */
  val tools = new Tools(V, E, edges.clone, Vemb, Eemb, edgesEmb.clone)

  val answerArray = new Array[Long](V+1)
  val (side, sideEmb) = (tools.side, tools.sideEmb)
  for (i <- 1 to V) {
    val vol = if (i % side == 0) side else i % side
    answerArray(i) = ((i-1) / side) * sideEmb + vol
  }

  val answer: Answer = Answer(V, Vemb, sideEmb).fromArray(answerArray)

  /* output */
  printer(answer, V)
}

class Tools(
  val V: Int, val E: Int,
  val edges: Array[List[Int]],
  val Vemb: Int, val Eemb: Int,
  val edgesEmb: Array[List[Int]]
) {
  import Math._

  val side = sqrt(V).ceil.toInt  // 正方形に置く場合の一辺の長さ
  val sideEmb = sqrt(Vemb).round.toInt

  def checkScore(answer: Array[Long]) = ???
}

object MainFuncs {
  /* output */
  def printer(answer: Answer, V: Int): Unit = {
    import java.io._

    val pw = new PrintWriter(System.out)
    val answerArray = answer.toArray

    for (i <- 1 to V) {
      pw.println(s"1 ${answerArray(i)}")
    }
    pw.flush()
    pw.close()
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
