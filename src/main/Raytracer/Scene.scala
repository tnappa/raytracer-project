package Raytracer

import Raytracer.Constants._

import java.awt.Color
import scala.math._

class Scene {

  private var objectList = Vector[Object]()
  private var viewr: Option[Viewer] = None

  def objects = objectList
  def addObject(obj: Object): Unit = objectList = objectList :+ obj

  def addViewer(viewer: Viewer): Unit = this.viewr = Some(viewer)
  def viewer = viewr

  def tracePath(ray: LightRay, depth: Int): Color = {

    if (depth >= MaxDepth) return Color.BLACK

    var intersectingObjs = Vector[(Object, MyVector)]()

    for (obj <- objects) {
      val intersection = obj.intersection(ray)
      if (intersection.isDefined) {
        intersectingObjs = intersectingObjs :+ (obj, intersection.get)
      }
    }

    if (intersectingObjs.isEmpty) return Color.BLACK

    // choose object and intersection point closest to light rays origin
    val (obj, intersection) = intersectingObjs.minBy{ case (obj, intersect) => (ray.origin - intersect).lenght }
    val normal = obj.normal(intersection)

    val randomUnitVector = {
      val x = Random.nextGaussian
      val y = Random.nextGaussian
      val z = Random.nextGaussian
      val scalar = 1 / sqrt( pow(x, 2) + pow(y, 2) + pow(z, 2) )

      new MyVector(x, y, z) * scalar
    }

    // flips the vectors that are not in the hemisphere of the normal to get ray according to BRDF distribution for totally diffuse objects
    val direction = if (randomUnitVector * normal < 0) randomUnitVector * -1 else randomUnitVector

    val newRay = new LightRay(direction, intersection)

    // probability of the ray ( 1 / half the area of a unit sphere ) so that the surface doesn't reflect more light than it receives
    val p = 1 / (2 * Pi)

    // BRDF for totally diffuse surface
    val brdf = new MyColor(obj.color) * (1/Pi)

    val incoming = tracePath(newRay, depth + 1)

    // calculate color using lambertian reflectance equation
    (new MyColor(obj.color) * obj.emittance + (brdf * new MyColor(incoming) * ((direction * normal) / p))).asAwtColor
  }

}
