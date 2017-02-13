import org.scalajs.dom
import org.scalajs.dom.raw.{Event, HTMLImageElement}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

class ImageRepository(canvasContext: dom.CanvasRenderingContext2D) {

  private def imageWithSrc(src: String): HTMLImageElement = {
    val image = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    image.src = src
    image
  }

  private def onLoadFuture(img: HTMLImageElement) = {
    if (img.complete) {
      Future.successful(img.src)
    } else {
      val p = Promise[String]()
      img.onload = { (e: Event) =>
        p.success(img.src)
      }
      p.future
    }
  }

  val asteroid = imageWithSrc("images/asteroid.png")
  val background = imageWithSrc("images/background.png")
  val bullet = imageWithSrc("images/bullet.png")
  val ship = imageWithSrc("images/ship.png")
  val explosion = imageWithSrc("images/explosion.png")
  val alphabet = imageWithSrc("images/alphabet.png")

  val loaded: Future[Unit] = Future.sequence(Set(
    asteroid, background, bullet, ship, explosion, alphabet
  ).map(onLoadFuture(_))).map(_ => Unit)
}