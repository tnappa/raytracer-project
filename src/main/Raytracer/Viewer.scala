package Raytracer

import Raytracer.Constants._
import Raytracer.Helpers.MyVector

import scala.math._

class Viewer(val position: MyVector, fac: MyVector) {

  val facing = fac.asUnit

  val up = new MyVector(0, 1, 0)
  val xStep = (up crossProduct facing).asUnit * -(2 * tan(Fov/2) / (Width - 1))
  val yStep = (xStep crossProduct facing).asUnit * -(2 * tan(Fov/2) / (Width - 1))

  // a vector that points from this.position to the (0, 0) pixel (top left) of the viewport
  val origin = facing + (xStep * -(Width.toDouble / 2)) + (yStep * -(Heigth.toDouble / 2))

  // generates a white light ray that intersects the given x, y coordinates of the viewport
  def generateRay(x: Int, y: Int): LightRay = {
    val direction = origin + (xStep * x) + (yStep * y)
    new LightRay(direction, position)
  }

  val rays = Array.tabulate(Width, Heigth)((x, y) => generateRay(x, y))

}
