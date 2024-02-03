package Raytracer

import Raytracer.Helpers.MyVector

import java.awt.Color

class Wall(normal:        MyVector,
           val position:  MyVector
          ) extends Object {

  require(normal.length != 0, "Wall must have non-zero normal")

  private var col: Color   = Color.WHITE
  private var emt: Double  = 0.0
  private var smt: Boolean = true

  def color = col
  def emittance = emt
  def isSmooth = smt

  def setColor(c: Color) = col = c
  def setEmittance(e: Double) = emt = e
  def setSmoothness(s: Boolean) = smt = s

  def normal(position: MyVector): MyVector = normal.asUnit

  // Calculates intersection using line-plane intersection formula https://en.wikipedia.org/wiki/Line%E2%80%93plane_intersection
  // Only returns an intersection that is in the direction of the ray
  def intersection(ray: LightRay): Option[MyVector] = {
    val a = ray.direction * this.normal

    if (a == 0) None
    else {

      val distance = ((this.position - ray.origin) * this.normal) / a

      if (distance > 0) {
        Some(ray.origin + (ray.direction * distance))
      }

      else None
    }
  }

}
