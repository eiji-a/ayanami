
import org.scalatest.FunSuite

class Vector3Suite extends FunSuite {
  test("3d vector constructor") {
    val v1 = Vector3(1, 2, 3)
    assert(v1.x === 1.0)
    assert(v1.y === 2.0)
    assert(v1.z === 3.0)
    assert(v1.toString === "[1.0,2.0,3.0]")
  }

  test("3d vector calculation") {
    val v1 = Vector3(1, 2, 3)
    val v2 = Vector3(5, 7, 9)
    val v3 = v1 + v2
    assert(v3 === Vector3(6.0,9.0,12.0))
    val v4 = v1 - v2
    assert(v4 === Vector3(-4.0,-5.0,-6.0))
    val v5 = v3 * 2.4
    assert(v5 === Vector3(14.399999999999999,21.599999999999998,28.799999999999997))
    //val v6 = 1.2 ** v2
    //assert(v6.toString === "[14.399999999999999,21.599999999999998,28.799999999999997]")
    val s0 = v1 dot v2
    assert(s0 === 46.0)
    val s1 = v1 * v2
    assert(s1 === 46.0)
    val v7 = -v2
    assert(v7 === Vector3(-5.0,-7.0,-9.0))
    val v8 = v1 cross v2
    assert(v8 === Vector3(-3.0,6.0,-3.0))
    val v8_2 = v1 X v2
    assert(v8_2 === Vector3(-3.0,6.0,-3.0))
    val v9 = v1 + v2 * 2
    assert(v9 === Vector3(11.0,16.0,21.0))
    val s2 = v1.length
    assert(s2 === 3.7416573867739413)
    val s3 = v1.norm
    assert(s3 === 3.7416573867739413)
    val v10 = v1.normal
    assert(v10 === Vector3(0.2672612419124244,0.5345224838248488,0.8017837257372732))
    val v11 = Vector3(0, 0, 0).normal
    assert(v11.toString === "[NaN,NaN,NaN]")
    val s4 = (v2 - v1).length
    assert(s4 === 8.774964387392123)
    val v16 = Vector3(1, 2, 3)
    assert(v1 === v16)
  }

  test("3d vector constant") {
    val v12 = Vector3.O
    assert(v12 === Vector3(0.0,0.0,0.0))
    val v13 = Vector3.EX
    assert(v13 === Vector3(1.0,0.0,0.0))
    val v14 = Vector3.EY
    assert(v14 === Vector3(0.0,1.0,0.0))
    val v15 = Vector3.EZ
    assert(v15 === Vector3(0.0,0.0,1.0))
  }
}

