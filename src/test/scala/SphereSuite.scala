
import org.scalatest.FunSuite
import Math.{sqrt,abs}

class SphereSuite extends FunSuite {
  test("shape sphere constructor") {
    val s1 = Sphere(Vector3(1,1,1), 3)
    assert(s1.c === Vector3(1,1,1))
    assert(s1.r === 3.0)
  }

  test("shape sphere side") {
    val s1 = Sphere(Vector3(1,1,1), 3)
    assert(s1.side(Vector3(0,0,0)) < 0.0)
    assert(s1.side(Vector3(3,3,3)) > 0.0)
    assert(s1.side(Vector3(1,4,1)) === 0.0)
  }

  test("shape sphere distance") {
    val s1 = Sphere(Vector3(1,1,1), 3)
    val r1 = Ray(Vector3(0,0,0), Vector3(0,1,0))
    val t1 = s1.distance(r1)
    assert(t1.length === 2)
    assert(t1 === List[Double](1.0 - sqrt(7.0), 1.0 + sqrt(7.0)))
    val r2 = Ray(Vector3(0,5,0), Vector3(0,1,0))
    val t2 = s1.distance(r2)
    assert(t2.length === 2)
    assert(t2 === List[Double](-4.0 - sqrt(7.0), -4.0 + sqrt(7.0)))
    val r3 = Ray(Vector3(0,4,0), Vector3(1,0,1))
    val t3 = s1.distance(r3)
    assert(t3.length === 1)
    assert(abs(t3.head - sqrt(2.0)) < 0.000001)
    val r4 = Ray(Vector3(0,5,0), Vector3(1,0,1))
    val t4 = s1.distance(r4)
    assert(t4.length === 0)
    assert(t4 === List[Double]())
  }
}
