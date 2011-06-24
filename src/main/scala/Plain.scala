

case class Plain(nc: Vector3, dc: Double) {
  val d: Double = dc / nc.length
  val n: Vector3 = nc.normal

  def side(p: Vector3) = n * p + d
  def distance(r: Ray) = {
    val c = n * r.dir
    c match {
      case 0.0 => List[Double]()
      case _   => List[Double]((d + n * r.pos) / -c)
    }
  }

}
