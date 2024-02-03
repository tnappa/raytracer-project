package Raytracer

import Raytracer.Constants._
import Raytracer.Helpers.MyVector

import scala.math._

class Viewer(val position: MyVector, _facing: MyVector) {

  val facing = _facing.asUnit

  // calculate x and y distance of viewport pixels
  private val up = new MyVector(0, 1, 0)
  private def func(v: MyVector) = (v crossProduct facing).asUnit * -(2 * tan(Fov/2) / (Width - 1))
  private val xStep = func(up)
  private val yStep = func(xStep)

  // a vector that points from this.position to the (0, 0) pixel (top left) of the viewport
  private val origin = facing + (xStep * -(Width.toDouble / 2)) + (yStep * -(Heigth.toDouble / 2))

  // generates a white light ray that intersects the given x, y coordinates of the viewport
  private def generateRay(x: Int, y: Int): LightRay = {
    val direction = origin + (xStep * x) + (yStep * y)
    new LightRay(direction, position)
  }

  // calculate rays beforehand
  val rays = Array.tabulate(Width, Heigth)((x, y) => generateRay(x, y))

}
