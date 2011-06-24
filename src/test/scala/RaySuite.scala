
import org.scalatest.FunSuite

class RaySuite extends FunSuite {
  test("ray constructor") {
    val r1 = Ray(Vector3(1,2,3), Vector3(3,2,1))
    assert(r1.pos.toString === "[1.0,2.0,3.0]")
    assert(r1.dir.toString === "[0.8017837257372732,0.5345224838248488,0.2672612419124244]")
  }

  test("ray calc target") {
    val r1 = Ray(Vector3(1,2,3), Vector3(1,1,1))
    assert(r1.dir.toString === "[0.5773502691896258,0.5773502691896258,0.5773502691896258]")
    val v1 = r1.target(3.0)
    assert(v1.toString === "[2.7320508075688776,3.7320508075688776,4.732050807568878]")
  }

}
