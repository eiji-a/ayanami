

case class Matrix3(a11: Double, a12: Double, a13: Double,
                   a21: Double, a22: Double, a23: Double,
                   a31: Double, a32: Double, a33: Double) {

  // operator overload
  //

  def +(m: Matrix3) = Matrix3(
    a11 + m.a11, a12 + m.a12, a13 + m.a13,
    a21 + m.a21, a22 + m.a22, a23 + m.a23,
    a31 + m.a31, a32 + m.a32, a33 + m.a33)
  def -(m: Matrix3) = Matrix3(
    a11 - m.a11, a12 - m.a12, a13 - m.a13,
    a21 - m.a21, a22 - m.a22, a23 - m.a23,
    a31 - m.a31, a32 - m.a32, a33 - m.a33)
  def *(s: Double) = Matrix3(
    s * a11, s * a12, s * a13,
    s * a21, s * a22, s * a23,
    s * a31, s * a32, s * a33)
  def /(s: Double) = s match {
    case 0.0 => throw new ArithmeticException("/ by zero")
    case _   => this * (1.0 / s)
  }

  def *(v: Vector3) = Vector3(
    a11 * v.x + a12 * v.y + a13 * v.z,
    a21 * v.x + a22 * v.y + a23 * v.z,
    a31 * v.x + a32 * v.y + a33 * v.z)

  def *(m: Matrix3) = Matrix3(
    a11 * m.a11 + a12 * m.a21 + a13 * m.a31,
    a11 * m.a12 + a12 * m.a22 + a13 * m.a32,
    a11 * m.a13 + a12 * m.a23 + a13 * m.a33,
    a21 * m.a11 + a22 * m.a21 + a23 * m.a31,
    a21 * m.a12 + a22 * m.a22 + a23 * m.a32,
    a21 * m.a13 + a22 * m.a23 + a23 * m.a33,
    a31 * m.a11 + a32 * m.a21 + a33 * m.a31,
    a31 * m.a12 + a32 * m.a22 + a33 * m.a32,
    a31 * m.a13 + a32 * m.a23 + a33 * m.a33)

  //function
  //
  override def toString = "[[" + a11 + "," + a12 + "," + a13 + "]¥n" +
                          " [" + a21 + "," + a22 + "," + a23 + "]¥n" +
                          " [" + a31 + "," + a32 + "," + a33 + "]]"
  def determinant =
    a11 * a22 * a33 + a21 * a32 * a13 + a31 * a12 * a23
    - a11 * a32 * a23 - a31 * a22 * a13 - a21 * a12 * a33
  def transpose = Matrix3(
    a11, a21, a31,
    a12, a22, a32,
    a13, a23, a33)
  def inverse = {
    val det = this.determinant
    Matrix3(
      a22 * a33 - a23 * a32, a13 * a32 - a12 * a33, a12 * a23 - a13 * a22,
      a23 * a31 - a21 * a33, a11 * a33 - a13 * a31, a13 * a21 - a11 * a23,
      a21 * a32 - a22 * a31, a12 * a31 - a11 * a32, a11 * a22 - a12 * a21) / det
  }
}

object Matrix3 {
  val O = Matrix3(
    0.0, 0.0, 0.0,
    0.0, 0.0, 0.0,
    0.0, 0.0, 0.0)
  val E = Matrix3(
    1.0, 0.0, 0.0,
    0.0, 1.0, 0.0,
    0.0, 0.0, 1.0)

}
