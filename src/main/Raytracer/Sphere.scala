package Raytracer

import Raytracer.Helpers.MyVector

import java.awt.Color
import scala.math._

class Sphere(val center:    MyVector,
             val radius:    Double,
            ) extends Object {

  override def toString: String = s"center: ${center.toString}, radius: ${radius}"

  private var col: Color   = Color.WHITE
  private var emt: Double  = 0.0
  private var smt: Boolean = true

  def color = col
  def emittance = emt
  def isSmooth = smt

  def setColor(c: Color) = col = c
  def setEmittance(e: Double) = emt = e
  def setSmoothness(s: Boolean) = smt = s

  def normal(position: MyVector): MyVector = {
    position - this.center
  }

  // Calculates intersection using line-sphere intersection formula https://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection
  // returns only the closest intersection in the direction of the ray that is not the origin of the ray
  def intersection(ray: LightRay): Option[MyVector] = {
    val unitRay = ray.asUnit
    val determinant = pow( unitRay * (ray.origin - this.center), 2 ) - (pow( (ray.origin - this.center).length, 2 ) - pow(this.radius, 2))

    if (determinant >= 0) {
      val distance1 = (unitRay * (ray.origin - this.center)) * -1 - sqrt(determinant)
      val distance2 = distance1 + 2 * sqrt(determinant)

      // choose intersection closest to origin of light ray and in the direction of the light ray
      if (distance1 > 0 || distance2 > 0) {

        val distance = {
          if (distance1 > 0 && distance2 > 0) {
            min(distance1, distance2)
          } else max(distance1, distance2)
        }

        Some((unitRay * distance) + ray.origin)
      }
      else None
    }
    else None
  }

}
