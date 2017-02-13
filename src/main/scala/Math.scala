



case class Vector(x: Double, y: Double) {
  def +(other: Vector) = Vector(x + other.x, y + other.y)
  def -(other: Vector) = Vector(x - other.x, y - other.y)
  def *(a: Double) = Vector(x * a, y * a)
  def magnitude = Math.sqrt(x * x + y * y)
  def direction = Math.atan(y / x)
  def unit = Vector(x / magnitude, y / magnitude)
}

object Vector {
  val zero = Vector(0,0)
  def apply(magnitude: Double, direction: Direction): Vector = Vector(
    x = magnitude * Math.cos(direction.radians),
    y = magnitude * Math.sin(direction.radians)
  )
}

case class AngularVelocity(radians: Double) extends AnyVal with Ordered[AngularVelocity] {
  def +(other: AngularVelocity) = AngularVelocity(radians + other.radians)
  def -(other: AngularVelocity) = AngularVelocity(radians - other.radians)
  def compare(that: AngularVelocity): Int = radians.compare(that.radians)
  def attenuatedBy(factor: Double) = AngularVelocity(radians * factor)
}

case class Direction(radians: Double) {
  private def normalize = {
    val x = radians  % (2 * Math.PI)
    if(x > 0) {
      Direction(x)
    } else {
      Direction(x + Math.PI * 2)
    }
  }
  def +(velocity: AngularVelocity) = Direction(radians + velocity.radians).normalize
  def -(velocity: AngularVelocity) = Direction(radians - velocity.radians).normalize
}

object Direction {
  def right = Direction(0)
  def up =    Direction(Math.PI / 2 * 3)
  def down =  Direction(Math.PI / 2)
  def left =  Direction(Math.PI)
}

