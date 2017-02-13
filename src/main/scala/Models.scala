
case class Size(scale: Double) extends AnyVal {
  def *(f: Double) = Size(scale * f)
}

trait Rotatable {
  def angularVelocity: AngularVelocity
  def direction: Direction

  def updatedDirection = direction + angularVelocity
}

trait Massive extends Particle {
  def radius: Double
  def collidedWith(other: Massive) = (position - other.position).magnitude < radius + other.radius
}

trait Expiring {
  def lifetime: Int
  def isExpired = lifetime <= 0
}

trait Particle {

  def position: Vector
  def velocity: Vector

  def updatedPosition(implicit gameArea: GameArea) =
    if (position.x < 0)
      Vector(gameArea.width, position.y)
    else if (position.x > gameArea.width)
      Vector(0, position.y)
    else if (position.y < 0)
      Vector(position.x, gameArea.height)
    else if (position.y > gameArea.height)
      Vector(position.x, 0)
    else
      position + velocity
}

case class Score(value: Int) extends AnyVal {
  def +(other: Score) = Score(value + other.value)
}

case class Ship(position: Vector, direction: Direction, velocity: Vector, angularVelocity: AngularVelocity, cooldownRemaining: Int = 0) extends Massive with Rotatable {
  def radius = 17

  val bulletSpeed = 2
  val bulletCooldown = 5

  def acceleration = Vector(0.1, direction)

  def attemptSpawnedBullet = cooldownRemaining match {
    case 0 => (
      copy(cooldownRemaining = bulletCooldown),
      Some(Bullet(position, velocity + Vector(bulletSpeed, direction)))
      )
    case _ => (this, None)
  }

  def tick(implicit gameArea: GameArea) = copy(
    position = updatedPosition,
    direction = updatedDirection,
    angularVelocity = angularVelocity attenuatedBy 0.9,
    cooldownRemaining = Math.max(cooldownRemaining - 1, 0)
  )
}

case class Explosion(position: Vector, lifetime: Int = 30) extends Expiring {
  def tick = copy(lifetime = lifetime - 1)
}

case class Asteroid(position: Vector, size: Size, velocity: Vector, direction: Direction) extends Massive with Rotatable {
  def angularVelocity = AngularVelocity(Math.PI / 50)
  def radius = 25 * size.scale
  def tick(implicit gameArea: GameArea) = copy(position = updatedPosition, direction = updatedDirection)
  def score = Score((100 * size.scale).toInt)

  def debris:Set[Asteroid] = size.scale match {
    case s if s < 0.4 => Set()
    case s            =>
      val debrisTemplate = copy(size = size * 0.5)
      Set(
        debrisTemplate.copy(velocity = velocity + Vector(velocity.magnitude * 1.4, Direction(velocity.direction - Math.PI))),
        debrisTemplate.copy(velocity = velocity + Vector(velocity.magnitude * 1.4, Direction(velocity.direction - Math.PI / 3))),
        debrisTemplate.copy(velocity = velocity + Vector(velocity.magnitude * 1.4, Direction(velocity.direction + Math.PI / 3)))
      )

  }
}

case class Bullet(position: Vector, velocity: Vector, lifetime: Int = 60 * 2) extends Massive with Expiring {
  def radius = 5
  def tick(implicit gameArea: GameArea) = copy(
    position = updatedPosition,
    lifetime = Math.max(lifetime - 1, 0)
  )
}
