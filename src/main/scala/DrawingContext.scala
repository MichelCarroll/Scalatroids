import org.scalajs.dom

trait GameArea {
  val width: Int
  val height: Int
  def center = Vector(width / 2, height / 2)
}

class DrawingContext(canvasContext: dom.CanvasRenderingContext2D) extends GameArea {

  val imageRepository = new ImageRepository(canvasContext)
  val width = 600
  val height = 400

  canvasContext.canvas.width = width
  canvasContext.canvas.height = height

  def ready = imageRepository.loaded

  def drawShip(ship: Ship): Unit = {
    val w = 35
    val h = 35

    canvasContext.save()
    canvasContext.translate(ship.position.x, ship.position.y)
    canvasContext.rotate(ship.direction.radians)
    canvasContext.drawImage(imageRepository.ship, 0, 0, 425, 426, -w / 2, -h / 2, w, h)
    canvasContext.restore()
  }

  val innerCharacterWidth = 15
  val alphabetIndexMapping = (
    ('a' to 'z').zip(0 to 25)
      ++ ('0' to '9').zip(26 to 35)
    ).toMap

  def drawTextAlignLeft(position: Vector, text: String): Unit = {
    drawText(Vector(position.x - innerCharacterWidth / 2, position.y), text)
  }

  def drawTextCentered(position: Vector, text: String): Unit = {
    drawText(Vector(position.x - text.length / 2 * innerCharacterWidth - innerCharacterWidth / 2, position.y), text)
  }

  def drawTextAlignRight(position: Vector, text: String): Unit = {
    drawText(Vector(position.x - text.length * innerCharacterWidth - innerCharacterWidth / 2, position.y), text)
  }

  private def drawText(position: Vector, text: String): Unit = {
    val imageWidth = 20
    val imageHeight = 20

    canvasContext.save()
    canvasContext.translate(position.x, position.y)

    text
      .toLowerCase
      .map(alphabetIndexMapping.get)
      .foreach {
        case Some(index) =>
          canvasContext.translate(innerCharacterWidth, 0)
          canvasContext.drawImage(imageRepository.alphabet, index * 200, 0, 200, 200, -imageWidth / 2, -imageHeight / 2, imageWidth, imageHeight)

        case _ =>
          canvasContext.translate(innerCharacterWidth, 0)
      }

    canvasContext.restore()
  }

  def drawAsteroid(asteroid: Asteroid): Unit = {
    val w = 75 * asteroid.size.scale
    val h = 75 * asteroid.size.scale

    canvasContext.save()
    canvasContext.translate(asteroid.position.x, asteroid.position.y)
    canvasContext.rotate(asteroid.direction.radians)
    canvasContext.drawImage(imageRepository.asteroid, 0, 0, 636, 636, -w / 2, -h / 2, w, h)
    canvasContext.restore()
  }

  def drawExplosion(explosion: Explosion): Unit = {
    val w = 40
    val h = 40
    val animationStage = explosion.lifetime / 10

    canvasContext.save()
    canvasContext.translate(explosion.position.x, explosion.position.y)
    canvasContext.drawImage(imageRepository.explosion, 850 * animationStage, 0, 850, 800, -w / 2, -h / 2, w, h)
    canvasContext.restore()
  }

  def drawBullet(bullet: Bullet): Unit = {
    val w = 10
    val h = 10

    canvasContext.save()
    canvasContext.translate(bullet.position.x, bullet.position.y)
    canvasContext.drawImage(imageRepository.bullet, 0, 0, 188, 188, -w / 2, -h / 2, w, h)
    canvasContext.restore()
  }

  def background(): Unit = {
    val tileDimension = 100
    val numXBackgroundTiles = width / tileDimension + 1
    val numYBackgroundTiles = height / tileDimension + 1

    for(
      x <- 0 until numXBackgroundTiles;
      y <- 0 until numYBackgroundTiles
    ) {
      canvasContext.drawImage(imageRepository.background, 0, 0, 800, 788, x * tileDimension, y * tileDimension, 100, 100)
    }
  }

  def clear(): Unit = {
    canvasContext.fillStyle = "white"
    canvasContext.fillRect(0, 0, width, height)
  }

}

