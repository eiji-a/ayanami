import Math.{sqrt}

case class Vector2(x: Double, y: Double) {

  //operator overload
  //
  def +(v: Vector2) = Vector2(x + v.x, y + v.y)
  def -(v: Vector2) = Vector2(x - v.x, y - v.y)
  def *(s: Double)  = Vector2(x * s, y * s)
  def /(s: Double)  = {
    s match {
      case 0.0 => throw new ArithmeticException("/ by zero")
      case _   => this * (1.0 / s)
    }
  }

  //implicit def **(s: Double) = new Vector2(x * s, y * s, z * s)
  def dot(v: Vector2) = x * v.x + y * v.y
  def *(v: Vector2) = this.dot(v)
  def unary_- = Vector2(-x, -y)

  //functions
  //
  override def toString = "[" + x + "," + y + "]"
  def square = this * this
  def isZero = this.equals(Vector2.O)
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

object Vector2 {
  val O  = Vector2(0.0, 0.0)
  val EX = Vector2(1.0, 0.0)
  val EY = Vector2(0.0, 1.0)
}

