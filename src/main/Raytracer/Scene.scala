package Raytracer

import Raytracer.Constants._

import java.awt.Color
import javax.swing.JFrame

class Scene {

  private var objectList = Vector[Object]()
  private var viewr: Option[Viewer] = None

  def objects = objectList
  def addObject(obj: Object): Unit = objectList = objectList :+ obj

  def addViewer(viewer: Viewer): Unit = this.viewr = Some(viewer)
  def viewer = viewr

  // heavily inspired by https://en.wikipedia.org/wiki/Path_tracing#Algorithm
  def tracePath(ray: LightRay, prevObj: Option[Object], depth: Int): Color = {

    // if ray bounced enough times return black
    if (depth >= MaxDepth) return Color.BLACK

    var intersectingObjs = Vector[(Object, MyVector)]()

    for (obj <- objects) {
      val intersection = obj.intersection(ray)
      if (intersection.isDefined && !prevObj.contains(obj)) {
        intersectingObjs = intersectingObjs :+ (obj, intersection.get)
      }
    }

    // return black if ray doesn't intersect anything
    if (intersectingObjs.isEmpty) return Color.BLACK

    // choose object and intersection point closest to light rays origin
    val (obj, intersection) = intersectingObjs.minBy{ case (obj, intersect) => (ray.origin - intersect).lenght }
    val normal = obj.normal(intersection)

    // calculate reflected ray by randomly picking a ray in the hemisphere of normal
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

    // calculate reflected ray using Snell's law
    def renderMirror(normal: MyVector, intersection: MyVector, incoming: MyVector): LightRay = {
      val direction = incoming + (normal * 2 * ((normal * -1) * incoming))
      new LightRay(direction, intersection)
    }

    val newRay =
      if (obj.isSmooth) renderDiffuse(normal, intersection)
      else renderMirror(normal, intersection, ray.direction)

    val incoming = tracePath(newRay, Some(obj), depth + 1)

    // reflected ray's color = color emitted by obj + color reflected by obj
    // each colors maximum value is limited to 255
    ((new MyColor(obj.color) * obj.emittance) + (new MyColor(obj.color) * new MyColor(incoming))).asAwtColor

  }

  // renders scene by changing values corresponding to each pixel in imgArray and update frame each iteration
  def renderScene(imgArray: Array[Int], frame: JFrame): Unit = {
    require(viewer.isDefined, "Viewer must be defined")

    for {
      y <- (0 until Heigth)
      x <- (0 until Width)
    } {
      imgArray(y * Width + x) = computeColor(x, y, imgArray)
      frame.repaint()
    }

  }

  // compute color for each ray corresponding to each pixel in the final image
  def computeColor(x: Int, y: Int, imgArray: Array[Int]): Int = {
    var rVal = 0
    var gVal = 0
    var bVal = 0
    for (i <- 0 until NumOfSamples) {
      val ray = viewer.get.generateRay(x, y)
      val color = tracePath(ray, None, 0)
      rVal += color.getRed
      gVal += color.getGreen
      bVal += color.getBlue
    }
    val red = rVal / NumOfSamples
    val green = gVal / NumOfSamples
    val blue = bVal / NumOfSamples
    imgArray(y * Width + x) = (red << 16) + (green << 8) + blue
    println(s"${x}, ${y}: ${red} ${green} ${blue}")

    (red << 16) + (green << 8) + blue
    }

}
