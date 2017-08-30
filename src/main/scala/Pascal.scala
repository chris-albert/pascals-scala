import org.scalameter.Key
import org.scalameter.Warmer
import org.scalameter._

object Pascal {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> false
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]) = {

    val seqtime = standardConfig measure {
      getTriangle(100000)
    }
    println(s"functions pascal: $seqtime")

    val partime = standardConfig measure {
      getTriangleMutable(100000)
    }
    println(s"imperitive pascals: $partime")
  }

  def getTriangle(height: Int): List[List[Int]] =
    (1 until height).foldLeft(List(List(1))){case (prev,_) => genRow(prev.head) :: prev}.reverse

  def genRow(prevRow: List[Int]): List[Int] =
    1 :: prevRow.sliding(2).map(_.sum).toList ::: (if(prevRow.size > 1) List(1) else List())

  def getTriangleMutable(height: Int): Array[Array[Int]] = {
    val arr = Array.ofDim[Int](height,height)
    var row = 1
    var col = 1
    arr(0)(0) = 1
    while(row < height) {
      arr(row)(0) = 1
      while(col < height && col < row + 1) {
        arr(row)(col) = arr(row - 1)(col - 1) + arr(row - 1)(col)
        col = col + 1
      }
      col = 1
      row = row + 1
    }

    arr
  }

  def renderTriangle(triangle: Array[Array[Int]]): String = {
    val maxDigit = 2
    val sb = new StringBuilder
    for(i <- triangle.indices) {
      val row = triangle(i)
      addSpace(sb,(triangle.length - 1 - i) * 2)
      for(r <- (0 until i + 1)) {
        val col = row(r)
        if(col != 0) {
          sb.append(col)
        }
        if(r < i) {
          addSpace(sb,2)
        }
      }
      if(i < triangle.length - 1) {
        sb.append("\n")
      }
    }
    sb.toString
  }

  def addSpace(sb: StringBuilder,count: Int): StringBuilder = {
    (0 until count).foreach(_ => sb.append(" "))
    sb
  }
}
