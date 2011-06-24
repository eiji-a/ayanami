

case class Ray(p: Vector3, d: Vector3) {
  val pos: Vector3 = p
  val dir: Vector3 = d.normal

  def target(t: Double) = pos + (dir * t)
}
