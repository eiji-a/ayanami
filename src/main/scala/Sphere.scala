
import Math.{sqrt}

// SPHERE: |c - p| = r^2
//  where c is center of sphere (cx,cy,cz)
//        p is point on the sphere (x,y,z)
//        r is radius of sphere

case class Sphere(c: Vector3, r: Double) {

  def side(p: Vector3) = (p - c).length - r
  def distance(r1: Ray) = {
    val o = c - r1.pos
    val t0 = o * r1.dir                     // projection C-p to d
    val t1 = r * r - (o.square - (t0 * t0)) // R^2 - (|C-p|^2 - ((C-p).d)^2)
    t1 match {
      case x if x <  0.0 => List[Double]()
      case x if x == 0.0 => List[Double](t0)
      case x if x >  0.0 => {
        val t2 = sqrt(t1)
        List[Double](t0 - t2, t0 + t2)
      }
    }
  }

}
