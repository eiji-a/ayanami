
import org.scalatest.FunSuite
import Math.{sqrt}

class PlainSuite extends FunSuite {
  test("shape plain constructor") {
    val p1 = Plain(Vector3(0,1,0), 0)
    assert(p1.n === Vector3(0,1,0))
    assert(p1.d === 0.0)

    val p2 = Plain(Vector3(3,4,5), 5)
    assert(p2.n === Vector3(3,4,5).normal)
    assert(p2.d === (1.0 / sqrt(2.0)))
  }

  test("shape plain side") {
    val p1 = Plain(Vector3(0,1,0), -2)
    assert(p1.side(Vector3(0,1.5,0)) === -0.5)
    assert(p1.side(Vector3(0,3,0)) === 1.0)
    assert(p1.side(Vector3(0,-2,0)) === -4.0)
  }

  test("shape plain distance") {
    val p = Plain(Vector3(0,1,0), 1)
    val r1 = Ray(Vector3(0,1,0), Vector3(1,-1,1))
    val t1 = p.distance(r1)
    assert(t1.length === 1)
    assert(t1.head - sqrt(12.0) < 0.000001)
    val r2 = Ray(Vector3(0,1,0), Vector3(1,0,0))
    val t2 = p.distance(r2)
    assert(t2.isEmpty === true)
  }

}
