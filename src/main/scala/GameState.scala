

sealed trait GameState {
  def applyPlayerCommands(playerCommands: Set[PlayerCommand])(implicit gameArea: GameArea): GameState
}

case object Splash extends GameState {
  def applyPlayerCommands(playerCommands: Set[PlayerCommand])(implicit gameArea: GameArea): GameState = playerCommands.foldRight(this: GameState) { (playerCommand, gameState) =>
    playerCommand match {
      case PlayerCommand.Acknowledge => Gaming.level(1)
      case _ => gameState
    }
  }
}

case class GameOver(score: Score) extends GameState {
  def applyPlayerCommands(playerCommands: Set[PlayerCommand])(implicit gameArea: GameArea): GameState = playerCommands.foldRight(this: GameState) { (playerCommand, gameState) =>
    playerCommand match {
      case PlayerCommand.Acknowledge => Gaming.level(1)
      case _ => gameState
    }
  }
}

case class Gaming(
                      level: Int,
                      ship: Ship,
                      bullets: Set[Bullet] = Set(),
                      asteroids: Set[Asteroid] = Set(),
                      explosions: Set[Explosion] = Set(),
                      score: Score = Score(0)

                    ) extends GameState {

  val angularAcceleration = AngularVelocity(0.01)
  val maxRotationVelocity = AngularVelocity(0.1)
  val minRotationVelocity = AngularVelocity(-0.1)


  def applyPlayerCommands(playerCommands: Set[PlayerCommand])(implicit gameArea: GameArea) = playerCommands.foldRight(this) { (playerCommand, gameState) =>
    import PlayerCommand._
    playerCommand match {

      case Deaccelerate => gameState.copy(
        ship = gameState.ship.copy(
          velocity = gameState.ship.velocity - gameState.ship.acceleration
        )
      )

      case Accelerate => gameState.copy(
        ship = gameState.ship.copy(
          velocity = gameState.ship.velocity + gameState.ship.acceleration
        )
      )

      case RotateClockwise => gameState.copy(
        ship = gameState.ship.copy(
          angularVelocity =
            if(gameState.ship.angularVelocity + angularAcceleration > maxRotationVelocity)
              maxRotationVelocity
            else
              gameState.ship.angularVelocity + angularAcceleration
        )
      )

      case RotateAnticlockwise => gameState.copy(
        ship = gameState.ship.copy(
          angularVelocity =
            if(gameState.ship.angularVelocity - angularAcceleration < minRotationVelocity)
              minRotationVelocity
            else
              gameState.ship.angularVelocity - angularAcceleration
        )
      )

      case Shoot =>
        val (ship, bulletOpt) = gameState.ship.attemptSpawnedBullet

        gameState.copy(
          ship = ship,
          bullets = gameState.bullets ++ (bulletOpt match {
            case Some(bullet) => Set[Bullet](bullet)
            case _ => Set[Bullet]()
          })
        )

      case _ => gameState
    }
  }

  def applyPhysics(implicit gameArea: GameArea) = copy(
    ship = ship.tick,
    bullets = bullets.map(_.tick),
    asteroids = asteroids.map(_.tick),
    explosions = explosions.map(_.tick)
  )

  def applyCollisions(implicit gameArea: GameArea) =
    if (asteroids.exists(_.collidedWith(ship)))
      GameOver(score = score)

    else {

      val bulletAsteroidCollision = bullets
        .map(bullet =>
          bullet -> asteroids.find(_.collidedWith(bullet))
        )
        .flatMap {
          case (bullet, Some(asteroid)) => Some(bullet, asteroid)
          case _ => None
        }

      val newScore = score + Score(bulletAsteroidCollision.map(_._2.score.value).sum)

      val newAsteroids = asteroids -- bulletAsteroidCollision.map(_._2) ++ bulletAsteroidCollision.flatMap(_._2.debris)

      if(newAsteroids.isEmpty)
        Gaming.level(level + 1).copy(score = newScore)
      else
        copy(
          bullets = bullets
            -- bulletAsteroidCollision.map(_._1),

          asteroids = newAsteroids,

          explosions = explosions ++ bulletAsteroidCollision
            .map { case (_, asteroid) => Explosion(asteroid.position) },

          score = newScore
        )

    }



  def applyExpirations = copy(
    bullets = bullets.filterNot(_.isExpired),
    explosions = explosions.filterNot(_.isExpired)
  )

}

case object Gaming {
  def level(n: Int)(implicit gameArea: GameArea) = {

    def initialAsteroids(n: Int): Set[Asteroid] =
      (1 to n).map(_ * Math.PI * 2 / n).map(angle =>
        Asteroid(
          position = gameArea.center + Vector(150, Direction(angle)),
          size = Size(1),
          velocity = Vector(1, Direction(angle)),
          direction = Direction.right
        )
      ).toSet

    Gaming(
      level = n,
      ship = Ship(
        position = gameArea.center,
        direction = Direction.right,
        velocity = Vector.zero,
        angularVelocity = AngularVelocity(0)
      ),
      asteroids = initialAsteroids(n)
    )
  }
}

object GameState {
  val initial: GameState = Splash
}

sealed trait PlayerCommand
object PlayerCommand {
  case object Accelerate extends PlayerCommand
  case object Deaccelerate extends PlayerCommand
  case object RotateClockwise extends PlayerCommand
  case object RotateAnticlockwise extends PlayerCommand
  case object Shoot extends PlayerCommand
  case object Acknowledge extends PlayerCommand

  def fromKeyCode(keyCode: Int): Option[PlayerCommand] = keyCode match {
    case 37 => Some(PlayerCommand.RotateAnticlockwise)
    case 38 => Some(PlayerCommand.Accelerate)
    case 39 => Some(PlayerCommand.RotateClockwise)
    case 40 => Some(PlayerCommand.Deaccelerate)
    case 32 => Some(PlayerCommand.Shoot)
    case 13 => Some(PlayerCommand.Acknowledge)
    case _  => None
  }
}