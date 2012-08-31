
import org.scalatest.FunSuite

class Matrix4Suite extends FunSuite {
  test("4d matrix constructor") {
    val m1 = Matrix4(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
    assert(m1.a11 === 1.0)
    assert(m1.a12 === 2.0)
    assert(m1.a13 === 3.0)
    assert(m1.a21 === 4.0)
    assert(m1.a22 === 5.0)
    assert(m1.a23 === 6.0)
    assert(m1.a31 === 7.0)
    assert(m1.a32 === 8.0)
    assert(m1.a33 === 9.0)
    assert(m1.a41 === 10.0)
    assert(m1.a42 === 11.0)
    assert(m1.a43 === 12.0)
    assert(m1.a44 === 13.0)
    assert(m1.toString === "[[1.0,2.0,3.0,0]¥n [4.0,5.0,6.0,0]¥n [7.0,8.0,9.0,0]¥n [10.0,11.0,12.0,13.0]]")
  }

  test("4d matrix calculation") {

  }

  test("4d matrix constants") {
  }

}
