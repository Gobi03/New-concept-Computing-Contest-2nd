import java.io._
import java.util.StringTokenizer
import CommonPackage._
import ToolsPackage._

object Main extends App {
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
  val result = Array.ofDim[Int](sideEmb+1, sideEmb+1)

  val answerArray = Array.fill[List[Int]](V+1)(Nil)
  for (i <- 1 to V) {
    val vol = if (i % side == 0) side else i % side
    answerArray(i) = ((i-1) / side) * sideEmb + vol :: answerArray(i)
  }

  val answer: Answer = Answer(result, sideEmb).fromArray(answerArray)


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
