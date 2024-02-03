package Raytracer

import Raytracer.Constants._
import Raytracer.Helpers.{MyColor, MyVector}

import scala.annotation.tailrec

class Scene {

  private var objectList = Vector[Object]()
  private var viewr: Option[Viewer] = None
  private var name_ = "Raytracer"
  private var renderThread: Option[Thread] = None

  def name = name_
  def setName(s: String): Unit = name_ = s

  def objects = objectList
  def addObject(obj: Object): Unit = objectList = objectList :+ obj

  def addViewer(viewer: Viewer): Unit = this.viewr = Some(viewer)
  def viewer = viewr

  // inspired by https://en.wikipedia.org/wiki/Path_tracing#Algorithm
  @tailrec private final def tracePath(ray: LightRay, prevObj: Option[Object], depth: Int): MyColor = {
    // if ray bounced enough times return black
    if (depth >= MaxDepth) return MyColor.BLACK

    var intersectingObjs = Vector[(Object, MyVector)]()

    for (obj <- objects) {
      val intersection = obj.intersection(ray)
      if (intersection.isDefined && !prevObj.contains(obj)) {
        intersectingObjs = intersectingObjs :+ (obj, intersection.get)
      }
    }

    // return black if ray doesn't intersect anything
    if (intersectingObjs.isEmpty) return MyColor.BLACK

    // choose object and intersection point closest to light rays origin
    val (obj, intersection) = intersectingObjs.minBy{ case (obj, intersect) => (ray.origin - intersect).length }
    val normal = obj.normal(intersection)

    // objects can either reflect or emmit light, but not both
    if (obj.emittance > 0.0) return MyColor(obj.color) * ray.color * obj.emittance

    // calculate diffuse reflection by randomly picking a ray in the hemisphere of normal
    def renderDiffuse(normal: MyVector, intersection: MyVector): LightRay = {
      val randomUnitVector = {
        val x = Random.nextGaussian
        val y = Random.nextGaussian
        val z = Random.nextGaussian

        new MyVector(x, y, z).asUnit
      }
      // flips the vectors that are not in the hemisphere of the normal to get ray according to BRDF distribution for totally diffuse objects
      val direction = if (randomUnitVector * normal < 0) randomUnitVector * -1 else randomUnitVector

      new LightRay(direction, intersection)
    }

    // calculate mirror reflection using Snell's law
    def renderMirror(normal: MyVector, intersection: MyVector, incoming: MyVector): LightRay = {
      val direction = incoming + (normal * 2 * ((normal * -1) * incoming))
      new LightRay(direction, intersection)
    }

    val newRay =
      if (obj.isSmooth) renderDiffuse(normal, intersection)
      else renderMirror(normal, intersection, ray.direction)

    // multiply incoming rays color with objects color to get color that is reflected
    newRay.setColor(ray.color * MyColor(obj.color))

    tracePath(newRay, Some(obj), depth + 1)
  }

  def startRenderThread(): Unit = {
    val t = new Thread(new RenderWorker)
    renderThread = Some(t)
    t.start()
  }

  def stopRenderThread(): Unit = {
    renderThread.foreach(_.interrupt())
  }

  def rerender(): Unit = {
    stopRenderThread()
    startRenderThread()
  }

  // run rendering in a separate thread from main
  private class RenderWorker extends Runnable {
    def run(): Unit = {
      renderScene()
    }
  }

  // renders scene by changing values corresponding to each pixel in imgArray and update frame each iteration
  private def renderScene(): Unit = {
    require(viewer.isDefined, "Viewer must be defined")

    println("rendering...")

    var i = 0
    while (i < Width * Heigth && !Thread.interrupted()) {
      var y = i / Heigth
      var x = i % Heigth
      imgArray(i) = computeColor(x, y)
      frame.repaint()
      i += 1
    }

    if (i == Width * Heigth) println("DONE") // only print when thread is not interrupted
  }

  // compute color for each ray corresponding to each pixel in the final image
  private def computeColor(x: Int, y: Int): Int = {
    var color = MyColor.BLACK
    val rays = viewer.get.rays
    for (i <- 0 until NumOfSamples) {
      val ray = rays(x)(y)
      color = color + tracePath(ray, None, 0)
    }
    val awtCol = (color * (1.0/ NumOfSamples)).asAwtColor
    val rgb = awtCol.getRGB
    awtCol.getRGB
    }

}
