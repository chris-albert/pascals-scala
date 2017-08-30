import org.scalatest.{Matchers, WordSpec}

class PascalSpec extends WordSpec with Matchers {

  "Pascal" should {
    "get triangle of height 1" in {
      Pascal.getTriangle(1) shouldBe List(List(1))
    }
    "get triangle of height 4" in {
      Pascal.getTriangle(4) shouldBe List(
        List(1),
        List(1,1),
        List(1,2,1),
        List(1,3,3,1)
      )
    }
    "get triangle of height 8" in {
      Pascal.getTriangle(8) shouldBe List(
        List(1), 
        List(1, 1), 
        List(1, 2, 1), 
        List(1, 3, 3, 1), 
        List(1, 4, 6, 4, 1), 
        List(1, 5, 10, 10, 5, 1), 
        List(1, 6, 15, 20, 15, 6, 1), 
        List(1, 7, 21, 35, 35, 21, 7, 1)
      )
    }
    "get triangle mutable of height 4" in {
      Pascal.getTriangleMutable(4) shouldBe Array(
        Array(1,0,0,0),
        Array(1,1,0,0),
        Array(1,2,1,0),
        Array(1,3,3,1)
      )
    }
  }

  "Render triangle" should {
    "only single space digits" in {
      Pascal.renderTriangle(Pascal.getTriangleMutable(5)) shouldBe
        """    1
          |   1 1
          |  1 2 1
          | 1 3 3 1
          |1 4 6 4 1""".stripMargin
    }
    "double space digits" in {
      println(Pascal.renderTriangle(Pascal.getTriangleMutable(6)))
      Pascal.renderTriangle(Pascal.getTriangleMutable(6)) shouldBe
        """           1
          |        1     1
          |      1    2    1
          |    1   3     3   1
          |  1   4    6    4   1
          |1   5   10   10   5   1""".stripMargin
    }
  }
}
