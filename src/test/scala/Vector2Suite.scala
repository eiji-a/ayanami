
import org.scalatest.FunSuite

class Vector2Suite extends FunSuite {
  test("2d vector constructor") {
    val v1 = Vector2(1, 2)
    assert(v1.x === 1.0)
    assert(v1.y === 2.0)
    assert(v1.toString === "[1.0,2.0]")
  }

  test("2d vector calculation") {
    val v1 = Vector2(1, 2)
    val v2 = Vector2(5, 7)
    val v3 = v1 + v2
    assert(v3 === Vector2(6.0,9.0))
    val v4 = v1 - v2
    assert(v4 === Vector2(-4.0,-5.0))
    val v5 = v3 * 2.4
    assert(v5 === Vector2(14.399999999999999,21.599999999999998))
    val v6 = v3 / 4.0
    assert(v6 === Vector2(1.5, 2.25))
    try {
      val v61 = v3 / 0.0
      fail("vector can not be devided by zero")
    } catch {
      case e: ArithmeticException => assert(true)
    }
    val s0 = v1 dot v2
    assert(s0 === 19.0)
    val s1 = v1 * v2
    assert(s1 === 19.0)
    val v7 = -v2
    assert(v7 === Vector2(-5.0,-7.0))
    val v9 = v1 + v2 * 2
    assert(v9 === Vector2(11.0,16.0))
    val s2 = v1.length
    assert(s2 === 2.23606797749979)
    val s3 = v1.norm
    assert(s3 === 2.23606797749979)
    val v10 = v1.normal
    assert(v10 === Vector2(0.4472135954999579, 0.8944271909999159))
    try {
      val v11 = Vector2(0, 0).normal
      fail("zero vector can not be normalize")
    } catch {
      case e: ArithmeticException => assert(true)
    }
    //assert(v11.toString === "[NaN,NaN]")
    val s4 = (v2 - v1).length
    assert(s4 === 6.4031242374328485)
    val v16 = Vector2(1, 2)
    assert(v1 === v16)
  }

  test("2d vector constant") {
    val v12 = Vector2.O
    assert(v12 === Vector2(0.0,0.0))
    val v13 = Vector2.EX
    assert(v13 === Vector2(1.0,0.0))
    val v14 = Vector2.EY
    assert(v14 === Vector2(0.0,1.0))
  }

}

