import Math.{sqrt}

case class Vector3(x: Double, y: Double, z: Double) {

  //operator overload
  //
  def +(v: Vector3) = Vector3(x + v.x, y + v.y, z + v.z)
  def -(v: Vector3) = Vector3(x - v.x, y - v.y, z - v.z)
  def *(s: Double)  = Vector3(x * s, y * s, z * s)
  def *(m: Matrix4) = {
    val v2 = Vector3(
      x * m.a11 + y * m.a21 + z * m.a31 + m.a41,
      x * m.a12 + y * m.a22 + z * m.a32 + m.a42,
      x * m.a13 + y * m.a23 + z * m.a33 + m.a43)
    m.a44 match { 
      case x if x == 1.0 => v2
      case _             => v2 / _
    }
  }
  def /(s: Double): Vector3  = s match {
    case 0.0 => throw new ArithmeticException("/ by zero")
    case _   => this * (1.0 / s)
  }
  //implicit def **(s: Double) = new Vector3(x * s, y * s, z * s)
  def dot(v: Vector3) = x * v.x + y * v.y + z * v.z
  def *(v: Vector3) = this.dot(v)
  def cross(v: Vector3) = Vector3(
    y * v.z - z * v.y,
    z * v.x - x * v.z,
    x * v.y - y * v.x)
  def X(v: Vector3) = this.cross(v)
  def unary_- = Vector3(-x, -y, -z)

  //function
  //
  override def toString = "[" + x + "," + y + "," + z + "]"
  def square = this * this
  def isZero = this.equals(Vector3.O)
  def length = sqrt(this.square)
  def norm = this.length
  def normal = {
    val len = this.length
    len match {
      case 0.0 => throw new ArithmeticException("length is zero")
      case _   => this / len
    }
  }

}

object Vector3 {
  val O  = Vector3(0.0, 0.0, 0.0)
  val EX = Vector3(1.0, 0.0, 0.0)
  val EY = Vector3(0.0, 1.0, 0.0)
  val EZ = Vector3(0.0, 0.0, 1.0)
}

