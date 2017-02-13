
import org.scalajs.dom
import org.scalajs.dom.html

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.timers._


@JSExport
object Game {

  @JSExport
  def main(canvas: html.Canvas): Unit = {

    var gameState = GameState.initial
    var activePlayerCommands: Set[PlayerCommand] = Set()

    implicit val drawingContext = new DrawingContext(canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D] )

    drawingContext.ready.map(_ => redraw())

    def redraw(): Unit = gameState match {

      case Gaming(level, ship, bullets, asteroids, explosions, score) =>
        drawingContext.clear()
        drawingContext.background()
        drawingContext.drawShip(ship)
        drawingContext.drawTextAlignRight(Vector(drawingContext.width - 10, 10), s"Score ${score.value}")
        bullets.foreach(drawingContext.drawBullet)
        asteroids.foreach(drawingContext.drawAsteroid)
        explosions.foreach(drawingContext.drawExplosion)

      case Splash =>
        drawingContext.clear()
        drawingContext.background()
        drawingContext.drawTextCentered(Vector(drawingContext.width / 2, drawingContext.height / 2 - 75), s"Welcome to Scalatroids!")
        drawingContext.drawTextCentered(Vector(drawingContext.width / 2, drawingContext.height / 2 - 25), s"Made by Michel Carroll")
        drawingContext.drawTextCentered(Vector(drawingContext.width / 2, drawingContext.height / 2 + 25), s"Arrows to Move and Space to Shoot")
        drawingContext.drawTextCentered(Vector(drawingContext.width / 2, drawingContext.height / 2 + 75), s"Press Enter to Continue")

      case GameOver(score) =>
        drawingContext.clear()
        drawingContext.background()
        drawingContext.drawTextCentered(Vector(drawingContext.width / 2, drawingContext.height / 2 - 50), s"Sorry you died")
        drawingContext.drawTextCentered(Vector(drawingContext.width / 2, drawingContext.height / 2), s"Final Score ${score.value}")
        drawingContext.drawTextCentered(Vector(drawingContext.width / 2, drawingContext.height / 2 + 50), s"Press Enter to Start Over")

    }


    dom.document.onkeydown = (e: dom.KeyboardEvent) => {
      activePlayerCommands = activePlayerCommands ++ PlayerCommand.fromKeyCode(e.keyCode)
        .map(Set(_))
        .getOrElse(Set())
    }

    dom.document.onkeyup = (e: dom.KeyboardEvent) => {
      activePlayerCommands = activePlayerCommands -- PlayerCommand.fromKeyCode(e.keyCode)
        .map(Set(_))
        .getOrElse(Set())
    }


    def update(): Unit =
      gameState =
        (gameState match {

          case gaming@Gaming(_, _, _, _, _, _) =>
            gaming
              .applyPhysics
              .applyExpirations
              .applyCollisions

          case _ => gameState

        })
        .applyPlayerCommands(activePlayerCommands)

    setInterval(1000 / 60) { //60 fps
      update()
      redraw()
    }

  }
}