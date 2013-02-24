

case class Matrix4(a11: Double, a12: Double, a13: Double,
                   a21: Double, a22: Double, a23: Double,
                   a31: Double, a32: Double, a33: Double,
                   a41: Double, a42: Double, a43: Double, a44: Double) {

  // operator overload
  //

  def +(m: Matrix4) = Matrix4(
    a11 + m.a11, a12 + m.a12, a13 + m.a13,
    a21 + m.a21, a22 + m.a22, a23 + m.a23,
    a31 + m.a31, a32 + m.a32, a33 + m.a33,
    a41 + m.a41, a42 + m.a42, a43 + m.a43, a44 + m.a44)
  def -(m: Matrix4) = Matrix4(
    a11 - m.a11, a12 - m.a12, a13 - m.a13,
    a21 - m.a21, a22 - m.a22, a23 - m.a23,
    a31 - m.a31, a32 - m.a32, a33 - m.a33,
    a41 - m.a41, a42 - m.a42, a43 - m.a43, a44 - m.a44)
  def *(s: Double) = Matrix4(
    s * a11, s * a12, s * a13,
    s * a21, s * a22, s * a23,
    s * a31, s * a32, s * a33,
    s * a41, s * a42, s * a43, s * a44)
  def /(s: Double) = s match {
    case 0.0 => throw new ArithmeticException("/ by zero")
    case _   => this * (1.0 / s)
  }

  def *(v: Vector3) = {
    val v2 = Vector3(
      a11 * v.x + a12 * v.y + a13 * v.z,
      a21 * v.x + a22 * v.y + a23 * v.z,
      a31 * v.x + a32 * v.y + a33 * v.z)
    a41 * v.x + a42 * v.y + a43 * v.z + a44 match {
      case x if x == 1.0 => v2
      case _             => v2 / _
    }
  }

  def *(m: Matrix4) = Matrix4(
    a11 * m.a11 + a12 * m.a21 + a13 * m.a31,
    a11 * m.a12 + a12 * m.a22 + a13 * m.a32,
    a11 * m.a13 + a12 * m.a23 + a13 * m.a33,
    a21 * m.a11 + a22 * m.a21 + a23 * m.a31,
    a21 * m.a12 + a22 * m.a22 + a23 * m.a32,
    a21 * m.a13 + a22 * m.a23 + a23 * m.a33,
    a31 * m.a11 + a32 * m.a21 + a33 * m.a31,
    a31 * m.a12 + a32 * m.a22 + a33 * m.a32,
    a31 * m.a13 + a32 * m.a23 + a33 * m.a33,
    a41 * m.a11 + a42 * m.a21 + a43 * m.a31,
    a41 * m.a12 + a42 * m.a22 + a43 * m.a32,
    a41 * m.a13 + a42 * m.a23 + a43 * m.a33,
    a44 * m.a44)

  //function
  //
  override def toString = "[[" + a11 + "," + a12 + "," + a13 + ",0]¥n" +
                          " [" + a21 + "," + a22 + "," + a23 + ",0]¥n" +
                          " [" + a31 + "," + a32 + "," + a33 + ",0]¥n" +
                          " [" + a41 + "," + a42 + "," + a43 + "," + a44 + "]]"
  def determinant =
    a11 * a22 * a33 * a44 + 0.0 + 0.0
    + 0.0 + a12 * a23 * a31 * a44 + 0.0
    + a13 * a21 * a32 * a44 + 0.0 + 0.0
    + 0.0 + 0.0 + 0.0
    - 0.0 - a11 * a23 * a32 * a44 - 0.0
    - a12 * a21 * a33 * a44 - 0.0 - 0.0
    - 0.0 - a13 * a22 * a31 * a44 - 0.0
    - 0.0 - 0.0 - 0.0
  def transpose = Matrix4(
    a11, a21, a31,
    a12, a22, a32,
    a13, a23, a33,
    0.0, 0.0, 0.0, 1.0)
  def inverse = {
    val det = this.determinant
    Matrix4(
      a22 * a33 * a44 - a23 * a32 * a44,
      a13 * a32 * a44 - a12 * a33 * a44,
      a12 * a23 * a44 - a13 * a22 * a44,
      a23 * a31 * a44 - a21 * a33 * a44,
      a11 * a33 * a44 - a13 * a31 * a44,
      a13 * a21 * a44 - a11 * a23 * a44,
      a21 * a32 * a44 - a22 * a31 * a44,
      a12 * a31 * a44 - a11 * a32 * a44,
      a11 * a22 * a44 - a12 * a21 * a44,
      a21 * a33 * a42 + a22 * a31 * a43 + a23 * a32 * a41 -
      a21 * a32 * a43 - a22 * a33 * a41 - a23 * a31 * a42,
      a11 * a32 * a43 + a12 * a33 * a41 + a13 * a31 * a42 -
      a11 * a33 * a42 - a12 * a31 * a43 - a13 * a32 * a41,
      a11 * a23 * a42 + a12 * a21 * a43 + a13 * a22 * a41 -
      a11 * a22 * a43 - a12 * a23 * a41 - a13 * a21 * a42,
      a11 * a22 * a33 + a12 * a23 * a31 + a13 * a21 * a32 -
      a11 * a23 * a32 - a12 * a21 * a33 - a13 * a22 * a31) / det
  }


}

object Matrix4 {
  val O = Matrix4(
    0.0, 0.0, 0.0,
    0.0, 0.0, 0.0,
    0.0, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.0)
  val E = Matrix4(
    1.0, 0.0, 0.0,
    0.0, 1.0, 0.0,
    0.0, 0.0, 1.0,
    0.0, 0.0, 0.0, 1.0)


}
