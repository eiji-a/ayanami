import Math.{sqrt}

case class Vector3(x: Double, y: Double, z: Double) {

  override def toString = "[" + x + "," + y + "," + z + "]"

  def +(v: Vector3) = Vector3(x + v.x, y + v.y, z + v.z)
  def -(v: Vector3) = Vector3(x - v.x, y - v.y, z - v.z)
  def *(s: Double)  = Vector3(x * s, y * s, z * s)
  def /(s: Double)  = Vector3(x / s, y / s, z / s)
  //implicit def **(s: Double) = new Vector3(x * s, y * s, z * s)
  def dot(v: Vector3) = x * v.x + y * v.y + z * v.z
  def *(v: Vector3) = this.dot(v)
  def cross(v: Vector3) = Vector3(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
  def X(v: Vector3) = this.cross(v)
  def unary_- = Vector3(-x, -y, -z)
  def square = this * this
  def length = sqrt(this.square)
  def norm = this.length
  def normal = this / this.length

}

object Vector3 {
  val O  = Vector3(0.0, 0.0, 0.0)
  val EX = Vector3(1.0, 0.0, 0.0)
  val EY = Vector3(0.0, 1.0, 0.0)
  val EZ = Vector3(0.0, 0.0, 1.0)
}

